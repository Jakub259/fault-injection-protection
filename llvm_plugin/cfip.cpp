#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/AtomicOrdering.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/SROA.h"
#include "llvm/Transforms/Utils/Cloning.h"

using namespace llvm;
#define DEBUG_TYPE "cfip"

namespace {

void getUsersRec(Value *const User, DenseSet<Value *> &OutSV) {
  std::vector<Value *> toProcess;
  toProcess.push_back(User);

  while (!toProcess.empty()) {
    Value *currentUser = toProcess.back();
    toProcess.pop_back();

    if (OutSV.insert(currentUser).second) {
      // taint all users
      for (auto *user : currentUser->users()) {
        toProcess.push_back(user);
      }
    }

    // it's impossible to tell what the function actually does with parameters
    // when it's just a declaration, thus we assume that all operands are
    // tainted
    if (auto ci = dyn_cast<CallBase>(currentUser)) {
      auto called_fn = ci->getCalledFunction();
      if (called_fn->isDeclaration())
        for (unsigned int i = 0; i < called_fn->getNumOperands(); ++i)
          toProcess.push_back(called_fn->getOperand(i));
    }

    // store instructions do not return the value, but rather store it
    // thus we need to taint the stored value
    if (auto st = dyn_cast<StoreInst>(currentUser)) {
      toProcess.push_back(st->getOperand(1));
    }
  }
}

std::string makeHardenedFunctionNameSuffix(SmallVector<unsigned> &args) {
  std::string suffix{".hardened"};
  for (auto arg : args) {
    suffix += "." + std::to_string(arg);
  }
  return suffix;
}

size_t harden_fn_args(Function *function,
                      DenseSet<Function *> *cloned_functions, bool atomic_state,
                      bool check_asap, SmallVector<unsigned> &args);

bool add_redundancy(llvm::Instruction *fault_detected_ptr, llvm::Value *user,
                    bool atomic_state, llvm::BasicBlock *error_bb,
                    DenseSet<Value *> *users_of_critical_values = nullptr,
                    DenseSet<Function *> *cloned_functions = nullptr) {
  // the cloned_functions must be present if users_of_critical_values is
  // present
  assert(!users_of_critical_values || cloned_functions);
  auto *instruction = dyn_cast<Instruction>(user);
  if (!instruction) {
    /* errs() << "Skipping non-instruction: " << *instruction << "\n"; */
    return false;
  }

  if (users_of_critical_values) {
    if (auto *call_inst = dyn_cast<CallBase>(instruction)) {
      errs() << "CALL: " << *call_inst << '\n';
      auto *called_fn = call_inst->getCalledFunction();
      if (called_fn->isDeclaration()) {
        errs() << "Skipping declaration: " << called_fn->getName() << "\n";
        return false;
      }
      SmallVector<unsigned> args{};

      for (unsigned i = 0; i < call_inst->getNumOperands(); i++) {
        auto *operand = call_inst->getOperand(i);
        if (users_of_critical_values->contains(operand)) {
          args.push_back(i);
        }
      }
      // the call does not use any critical values
      if (args.empty()) {
        return false;
      }

      // clone the function to harden
      const std::string cloned_function_name =
          (called_fn->getName() + makeHardenedFunctionNameSuffix(args)).str();
      auto M = called_fn->getParent();

      SmallVector<char> small_vec;
      auto *cloned_function = M->getFunction(cloned_function_name);
      // check if function is already cloned and hardend
      if (!cloned_function) {
        ValueToValueMapTy VMap;
        cloned_function = CloneFunction(called_fn, VMap);
        cloned_function->setName(cloned_function_name);
        cloned_functions->insert(cloned_function);
        harden_fn_args(cloned_function, cloned_functions, atomic_state,
                       static_cast<bool>(error_bb), args);
        cloned_function->setLinkage(GlobalValue::LinkageTypes::PrivateLinkage);
        errs() << "copied function for hardening: "
               << cloned_function->getName().str() << '\n';
      }

      call_inst->setCalledFunction(cloned_function);
    }
  }
  if (auto branch_inst = dyn_cast<llvm::BranchInst>(instruction)) {
    if (branch_inst->isConditional()) {
      errs() << "BR: " << *instruction << "\n";
      /*
// before :
if (cond)
{
  <true case>
}
else
{
  <false case>
}
// after:
chosen_case = !cond
if (cond)
{
  local_fault_detected = chosen_case != true
  <true case>
}
else
{
  local_fault_detected = chosen_case == true
  <false case>
}
*/
      auto *function = branch_inst->getFunction();
      auto *cond = branch_inst->getCondition();
      auto replace_successors_if_needed =
          [function, branch_inst](unsigned int i, StringRef name) {
            ValueToValueMapTy VMap;
            auto *bb = branch_inst->getSuccessor(i);
            if (bb->hasNPredecessorsOrMore(2)) {
              auto *cloned_bb = CloneBasicBlock(bb, VMap, name, function);
              remapInstructionsInBlocks({cloned_bb}, VMap);
              branch_inst->setSuccessor(i, cloned_bb);
              return cloned_bb;
            } else {
              return bb;
            }
          };

      auto *true_bb = replace_successors_if_needed(0, ".clone");
      auto *false_bb = replace_successors_if_needed(1, ".clone");
      // Create a variable to track the chosen branch
      IRBuilder<> builder(&function->getEntryBlock().front());
      auto *chosen_branch =
          builder.CreateAlloca(builder.getInt1Ty(), nullptr, "chosen_branch");

      builder.SetInsertPoint(branch_inst);

      // we set negation of cond, so that the target branch has to fix it
      builder.CreateStore(builder.CreateNeg(cond, "unexpected_result"),
                          chosen_branch);

      auto *i1_ty = builder.getInt1Ty();
      auto *i8_ty = builder.getInt8Ty();
      // Update true_bb and false_bb to include fault detection
      {
        builder.SetInsertPoint(&*true_bb->getFirstInsertionPt());
        auto *true_branch_fault = builder.CreateICmpNE(
            builder.CreateLoad(i1_ty, chosen_branch, false),
            ConstantInt::getTrue(i1_ty), "true_branch_fault");
        auto *true_branch_fault_ext =
            builder.CreateZExt(true_branch_fault, i8_ty);
        builder.CreateStore(
            builder.CreateOr(
                builder.CreateLoad(i8_ty, fault_detected_ptr, false),
                true_branch_fault_ext),
            fault_detected_ptr);
      }

      {
        builder.SetInsertPoint(&*false_bb->getFirstInsertionPt());
        auto *false_branch_fault = builder.CreateICmpNE(
            builder.CreateLoad(i1_ty, chosen_branch, false),
            ConstantInt::getFalse(i1_ty), "false_branch_fault");
        auto *false_branch_fault_ext =
            builder.CreateZExt(false_branch_fault, i8_ty);
        builder.CreateStore(
            builder.CreateOr(
                builder.CreateLoad(i8_ty, fault_detected_ptr, false),
                false_branch_fault_ext),
            fault_detected_ptr);
      }
    }

    return true;
  }

  Value *local_fault_detected{0};
  IRBuilder<> builder(instruction);
  if (isa<BinaryOperator>(instruction) or isa<UnaryOperator>(instruction) or
      isa<SelectInst>(instruction) or isa<CmpInst>(instruction) or
      isa<PHINode>(instruction)) {
    // before: x = a `op` b
    // after:
    //    result1 = a `op` b
    //    result2 = a `op` b
    //    local_fault_detected = result1 != result2
    //    fault_detected |= local_fault_detected

    const auto *inst_Ty = instruction->getType();

    const bool is_supported_by_cmp =
        inst_Ty->isIntOrIntVectorTy() || inst_Ty->isPtrOrPtrVectorTy();
    const bool is_supported_by_fcmp = inst_Ty->isFPOrFPVectorTy();

    const bool supported = is_supported_by_cmp || is_supported_by_fcmp;

    if (not supported)
      return false;

    errs() << "INST: " << *instruction << "\n";

    for (unsigned int i = 0; i < instruction->getNumOperands(); i++) {
      // "Undef values aren't exactly constants; if they have multiple uses,
      // they can appear to have different bit patterns at each use."

      // this is *unacceptable* for our purposes, so we replace it with a null
      if (auto operand = dyn_cast<UndefValue>(instruction->getOperand(i))) {
        errs() << "Replacing undef value with null: " << *operand << " in "
               << *instruction << "\n";
        instruction->setOperand(i, Constant::getNullValue(operand->getType()));
      }
    }

    auto *tmp1 = instruction->clone();
    tmp1->setName("tmp1");
    builder.Insert(tmp1);

    auto *tmp2 = instruction->clone();
    tmp2->setName("tmp2");
    builder.Insert(tmp2);

    // tmp1 != tmp2
    if (is_supported_by_cmp) {
      local_fault_detected =
          builder.CreateICmpNE(tmp1, tmp2, "is_fault_detected");
    } else if (is_supported_by_fcmp) {
      local_fault_detected =
          builder.CreateFCmpONE(tmp1, tmp2, "is_fault_detected");
    } else {
      assert(0 && "unsupported op");
    }
    // if it's vector we need to reduce it to scalar
    if (inst_Ty->isVectorTy()) {
      local_fault_detected = builder.CreateOrReduce(local_fault_detected);
    }

    local_fault_detected =
        builder.CreateZExt(local_fault_detected, builder.getInt8Ty());

    // TODO: Does it matter if we replace it with tmp1 or tmp2 or pick random?
    instruction->replaceAllUsesWith(tmp1);
    if (users_of_critical_values) {
      // Update the tracked values set to include the new instruction (tmp1)
      // and remove the old instruction that was replaced
      users_of_critical_values->erase(instruction);
      users_of_critical_values->insert(tmp1);
    }
    instruction->eraseFromParent();
  }
  if (!local_fault_detected)
    return false;

  // finalize
  Instruction *block_split_point, *state;
  builder.SetInsertPoint(
      cast<Instruction>(local_fault_detected)->getNextNode());

  if (atomic_state) {
    state = block_split_point = cast<Instruction>(builder.CreateAtomicRMW(
        AtomicRMWInst::BinOp::Or, fault_detected_ptr, local_fault_detected,
        MaybeAlign(1), AtomicOrdering::AcquireRelease));
  } else {
    // fault_detected |= local_fault_detected
    auto fault_detected =
        builder.CreateLoad(builder.getInt8Ty(), fault_detected_ptr, false);
    state = cast<Instruction>(
        builder.CreateOr(fault_detected, local_fault_detected));
    block_split_point = builder.CreateStore(state, fault_detected_ptr);
  }

  if (error_bb) {
    BasicBlock *before = state->getParent();
    BasicBlock *after =
        before->splitBasicBlock(block_split_point->getNextNode(), "after");

    auto old_terminator = before->getTerminator();
    IRBuilder<> Builder(old_terminator);

    auto *fault_detected_cond =
        Builder.CreateTrunc(state, builder.getInt1Ty(), "", true);
    Builder.CreateCondBr(
        fault_detected_cond, error_bb, after,
        MDBuilder(state->getContext()).createBranchWeights(1, 2000));
    old_terminator->eraseFromParent();
  }
  return true;
}

SmallVector<llvm::CallInst *, 1> find_values_to_harden(Function &function) {
  SmallVector<llvm::CallInst *, 1> opaque_calls;
  for (auto &basic_block : function)
    for (auto &instruction : basic_block) {
      if (auto call = dyn_cast<CallInst>(&instruction)) {
        if (auto *called_op = call->getCalledOperand()) {
          if (called_op->getName().starts_with("cfip_harden_var_"))
            opaque_calls.push_back(call);
        }
      }
    }
  return opaque_calls;
}

BasicBlock *insert_error_bb(Function &function) {
  auto &Ctx = function.getContext();
  // add a error handling basic block
  BasicBlock *error_bb = BasicBlock::Create(Ctx, "error_handling", &function);
  {
    IRBuilder<> builder(error_bb);
    builder.CreateCall(
        Intrinsic::getDeclaration(function.getParent(), Intrinsic::trap));
    builder.CreateUnreachable();
  }
  return error_bb;
}

void add_integrity_check(Function &function, Instruction *fault_detected_ptr,
                         bool atomic_state, BasicBlock *error_bb) {
  auto &Ctx = function.getContext();

  SmallVector<llvm::Instruction *, 1> return_instructions;
  for (auto &basic_block : function) {
    if (auto *ret_inst = dyn_cast<ReturnInst>(basic_block.getTerminator()))
      return_instructions.push_back(ret_inst);
  }
  // inserting checks before return instructions
  for (auto ret_inst : return_instructions) {
    BasicBlock *ret_bb = ret_inst->getParent();
    BasicBlock *new_ret_bb = ret_bb->splitBasicBlock(ret_inst, "ret_block");

    IRBuilder<> Builder(ret_bb->getTerminator());

    auto *fault_detected =
        Builder.CreateLoad(Builder.getInt8Ty(), fault_detected_ptr, false);
    if (atomic_state) {
      fault_detected->setAtomic(AtomicOrdering::Acquire);
    }

    auto *fault_detected_cond =
        Builder.CreateTrunc(fault_detected, Builder.getInt1Ty(), "", true);

    Builder.CreateCondBr(fault_detected_cond, error_bb, new_ret_bb,
                         MDBuilder(Ctx).createBranchWeights(1, 2000));
    ret_bb->getTerminator()->eraseFromParent();
  }
}
llvm::Value *unwrap_call(llvm::CallInst *opaque_call,
                         IRBuilder<> &alloca_builder) {
  llvm::IRBuilder<> value_unwrapper_builder(opaque_call);

  // rustc attribute hides value behind a call to prevent over-optimizations
  // it's no longer needed thus we unwrap them:
  // before: critical_var = opaque_call(start_value)
  // after: critical_var = start_value
  auto *called_function = opaque_call->getCalledFunction();

  // THIS WILL CAUSE A VERY SPECTACULAR CRASH IF LLVM FOR SOME REASON DECIDES
  // THAT SRET SHALL BE USED ON THE 2nd ARGUMENT INSTEAD OF THE 1st
  if (called_function->hasStructRetAttr()) {
    auto *sret_arg = opaque_call->getArgOperand(0);
    auto *byvalue_arg = opaque_call->getArgOperand(1);

    sret_arg->replaceAllUsesWith(byvalue_arg);
    opaque_call->eraseFromParent();
    return byvalue_arg;
  } else {
    auto *opaque_value_ptr = alloca_builder.CreateAlloca(
        opaque_call->getType(), nullptr, "unwrapped_value");
    value_unwrapper_builder.CreateStore(opaque_call->getOperand(0),
                                        opaque_value_ptr);

    llvm::LoadInst *critical_value_use = value_unwrapper_builder.CreateLoad(
        opaque_call->getType(), opaque_value_ptr, false, "replacement");

    opaque_call->replaceAllUsesWith(critical_value_use);
    opaque_call->eraseFromParent();
    return critical_value_use;
  }
}

llvm::Instruction *insert_bool(IRBuilder<> &alloca_builder, LLVMContext &Ctx) {

  auto *i8_type = llvm::IntegerType::getInt8Ty(Ctx);

  // create value to store state of program
  llvm::Instruction *fault_detected_ptr =
      alloca_builder.CreateAlloca(i8_type, nullptr, "fault_detected");
  alloca_builder.CreateStore(llvm::ConstantInt::get(i8_type, false),
                             fault_detected_ptr);

  alloca_builder.SetInsertPoint(fault_detected_ptr); // reset insert point
  return fault_detected_ptr;
}

size_t harden_fn_args(Function *function,
                      DenseSet<Function *> *cloned_functions, bool atomic_state,
                      bool check_asap, SmallVector<unsigned> &args) {
  LLVMContext &Ctx = function->getContext();

  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> alloca_builder(&function->getEntryBlock().front());

  auto *fault_detected_ptr = insert_bool(alloca_builder, Ctx);

  DenseSet<Value *> users_of_critical_values;
  for (const auto &arg : args) {
    getUsersRec(function->getArg(arg), users_of_critical_values);
  }
  if (users_of_critical_values.empty()) {
    return 0;
  }

  auto *error_bb = insert_error_bb(*function);

  std::vector worklist(users_of_critical_values.begin(),
                       users_of_critical_values.end());

  bool modified = false;
  for (auto *user : worklist) {
    modified |= add_redundancy(fault_detected_ptr, user, atomic_state,
                               check_asap ? error_bb : nullptr,
                               &users_of_critical_values, cloned_functions);
  }
  if (modified)
    add_integrity_check(*function, fault_detected_ptr, atomic_state, error_bb);

  return 1;
}

size_t harden_chosen(Function &function, DenseSet<Function *> *cloned_functions,
                     bool atomic_state, bool check_asap) {
  LLVMContext &Ctx = function.getContext();

  // In 99.999% cases there should be no more than one
  SmallVector<llvm::CallInst *, 1> opaque_calls =
      find_values_to_harden(function);

  if (opaque_calls.empty())
    return 0;

  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> alloca_builder(&function.getEntryBlock().front());

  auto *fault_detected_ptr = insert_bool(alloca_builder, Ctx);

  DenseSet<Value *> users_of_critical_values;
  for (auto &opaque_call : opaque_calls) {
    auto *critical_value_use = unwrap_call(opaque_call, alloca_builder);
    if (critical_value_use->use_empty()) {
      errs() << "WARNING: nothing to harden, value not used\n";
      continue;
    }

    getUsersRec(critical_value_use, users_of_critical_values);
  }

  if (users_of_critical_values.empty()) {
    return 0;
  }

  auto *error_bb = insert_error_bb(function);

  bool modified = false;
  for (auto *user : users_of_critical_values) {
    modified |= add_redundancy(fault_detected_ptr, user, atomic_state,
                               check_asap ? error_bb : nullptr,
                               &users_of_critical_values, cloned_functions);
  }
  if (modified)
    add_integrity_check(function, fault_detected_ptr, atomic_state, error_bb);

  return opaque_calls.size();
}

void finalize(Function &F, FunctionAnalysisManager &AM) {
  // invalidate analyses before cleanup passes
  AM.invalidate(F, PreservedAnalyses::none());
  // cleanup
  SROAPass(SROAOptions::ModifyCFG).run(F, AM);
  DCEPass().run(F, AM);

  // turn off optimizer otherwise our hardening will be optimized out
  F.addFnAttr(Attribute::OptimizeNone);
  // required be OptimizeNone
  F.addFnAttr(Attribute::NoInline);
  F.removeFnAttr(Attribute::OptimizeForDebugging);
  F.removeFnAttr(Attribute::OptimizeForSize);
  F.removeFnAttr(Attribute::MinSize);
  F.removeFnAttr(Attribute::AlwaysInline);
}

// only a heuristic
bool isLTOPhase(const Module &M) {
  unsigned compileUnitCount = 0;

  if (auto *CUs = M.getNamedMetadata("llvm.dbg.cu")) {
    compileUnitCount = CUs->getNumOperands();
    errs() << "Found " << compileUnitCount << " compile units\n";
  }

  return compileUnitCount > 1 || M.getModuleFlag("PIE Level");
}

template <auto Fn> struct Cfip : PassInfoMixin<Cfip<Fn>> {
  const bool atomic_state;
  const bool check_asap;
  const bool only_on_fullLTO;
  Cfip(bool atomic_state_ = false, bool check_asap_ = false,
       bool only_on_fullLTO_ = false)
      : atomic_state(atomic_state_), check_asap{check_asap_},
        only_on_fullLTO{only_on_fullLTO_} {}

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    errs() << "LTO " << isLTOPhase(M) << "\n";
    if (only_on_fullLTO) {
      // it appears that rustc only add this ModuleFlag when it's linking
      if (not isLTOPhase(M))
        return PreservedAnalyses::all();
    }

    bool modified = false;
    FunctionAnalysisManager *FAM =
        &AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
    DenseSet<Function *> cloned_functions;

    for (auto &F : M) {
      if (Fn(F, &cloned_functions, atomic_state, check_asap)) {
        finalize(F, *FAM);
        modified = true;
      }
    }

    for (auto F : cloned_functions) {
      finalize(*F, *FAM);
    }

    if (modified) {
      return PreservedAnalyses::none();
    } else {
      return PreservedAnalyses::all();
    }
  }

  static bool isRequired() { return true; }
};

size_t harden_all(Function &function,
                  [[maybe_unused]] DenseSet<Function *> *cloned_functions,
                  bool atomic_state, bool check_asap) {
  LLVMContext &Ctx = function.getContext();
  auto inst_cnt = function.getInstructionCount();
  if (!inst_cnt)
    return 0;

  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> alloca_builder(&function.getEntryBlock().front());

  auto *fault_detected_ptr = insert_bool(alloca_builder, Ctx);
  auto *error_bb = insert_error_bb(function);
  // add redundancy to all
  std::vector<llvm::Instruction *> instructions(inst_cnt);
  for (inst_iterator inst = inst_begin(function), end = inst_end(function);
       inst != end; ++inst) {
    instructions.push_back(&*inst);
  }

  bool modified = false;
  for (auto *i : instructions) {
    if (i) {
      // we are hardening everything we can in each function
      // thus we do not need to recurse into function calls
      modified |= add_redundancy(fault_detected_ptr, i, atomic_state,
                                 check_asap ? error_bb : nullptr);
    }
  }
  if (modified)
    add_integrity_check(function, fault_detected_ptr, atomic_state, error_bb);

  return 1;
}

struct CfipOpts {
  bool atomic_state;
  bool harden_all;
  bool check_asap;
  bool only_on_fullLTO;
};
Expected<CfipOpts> parseCfipOpts(StringRef Params) {
  CfipOpts Result{};
  while (!Params.empty()) {
    StringRef ParamName;
    std::tie(ParamName, Params) = Params.split(';');

    bool Enable = !ParamName.consume_front("no-");
    if (ParamName == "atomic-state")
      Result.atomic_state = Enable;
    else if (ParamName == "harden-all")
      Result.harden_all = Enable;
    else if (ParamName == "check-asap")
      Result.check_asap = Enable;
    else if (ParamName == "only-on-fullLTO")
      Result.only_on_fullLTO = Enable;
    else
      return make_error<StringError>("Unknown parameter: " + ParamName,
                                     inconvertibleErrorCode());
  }
  return Result;
}
} // namespace

llvm::PassPluginLibraryInfo getCfipPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "cfip", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (PassBuilder::checkParametrizedPassName(Name, "cfip")) {
                    auto Params = PassBuilder::parsePassParameters(
                        parseCfipOpts, Name, "cfip");
                    if (!Params) {
                      errs() << Params.takeError();
                      exit(EXIT_FAILURE);
                    }
                    if (Params->harden_all) {
                      MPM.addPass(Cfip<harden_all>{Params->atomic_state,
                                                   Params->check_asap,
                                                   Params->only_on_fullLTO});
                    } else {
                      MPM.addPass(Cfip<harden_chosen>{Params->atomic_state,
                                                      Params->check_asap,
                                                      Params->only_on_fullLTO});
                    }
                    return true;
                  }
                  return false;
                });
          }};
}
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getCfipPluginInfo();
}
