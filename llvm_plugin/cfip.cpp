#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/AtomicOrdering.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/SROA.h"

using namespace llvm;
#define DEBUG_TYPE "cfip"

namespace {

void getUsersRec(Value *const U, DenseSet<Value *> &OutSV) {
  OutSV.insert(U);
  for (auto *user : U->users()) {
    getUsersRec(user, OutSV);
  }
}

MDNode *create_br_weights(LLVMContext &Ctx, uint64_t br_weight_l,
                          uint64_t br_weight_r) {
  MDBuilder MDBuilder(Ctx);
  Metadata *branch_weights[] = {
      MDBuilder.createString("branch_weights"),
      MDBuilder.createString("expected"),
      ValueAsMetadata::get(
          ConstantInt::get(Type::getInt32Ty(Ctx), br_weight_l)),
      ValueAsMetadata::get(
          ConstantInt::get(Type::getInt32Ty(Ctx), br_weight_r)),
  };
  MDNode *ProfMD = MDNode::get(Ctx, branch_weights);
  return ProfMD;
}

void add_redundancy(llvm::Instruction *fault_detected_ptr, llvm::Value *value,
                    bool atomic_state, llvm::BasicBlock *error_bb) {
  // before: x = a + b
  // after:
  //    tmp1 = a + b
  //    tmp2 = a + b
  //    local_fault_detected = tmp1 != tmp2
  //    fault_detected |= local_fault_detected
  auto *instruction = dyn_cast<Instruction>(value);
  if (!instruction) {
    return;
  }

  if (isa<BinaryOperator>(instruction) or isa<UnaryOperator>(instruction) or
      isa<SelectInst>(instruction) or isa<CmpInst>(instruction)) {

    const auto *inst_Ty = instruction->getType();

    bool is_supported_by_cmp =
        inst_Ty->isIntOrIntVectorTy() || inst_Ty->isPtrOrPtrVectorTy();

    if (not is_supported_by_cmp)
      return;

    errs() << "Adding redundancy to: " << *instruction << "\n";

    IRBuilder<> builder(instruction);

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
    Value *local_fault_detected =
        builder.CreateICmpNE(tmp1, tmp2, "is_fault_detected");

    // if it's vector we need to reduce it to scalar
    if (inst_Ty->isVectorTy()) {
      local_fault_detected = builder.CreateOrReduce(local_fault_detected);
    }

    Instruction *last;
    Instruction *state;

    if (atomic_state) {
      local_fault_detected =
          builder.CreateZExt(local_fault_detected, builder.getInt8Ty());

      state = last = cast<Instruction>(builder.CreateAtomicRMW(
          AtomicRMWInst::BinOp::Or, fault_detected_ptr, local_fault_detected,
          MaybeAlign(1), AtomicOrdering::AcquireRelease));
    } else {
      local_fault_detected =
          builder.CreateZExt(local_fault_detected, builder.getInt8Ty());

      // fault_detected |= local_fault_detected
      auto fault_detected =
          builder.CreateLoad(builder.getInt8Ty(), fault_detected_ptr, false);

      local_fault_detected =
          builder.CreateZExt(local_fault_detected, builder.getInt8Ty());

      state = cast<Instruction>(
          builder.CreateOr(fault_detected, local_fault_detected));

      last = builder.CreateStore(state, fault_detected_ptr);
    }

    // TODO: Does it matter if we replace it with tmp1 or tmp2 or pick random?
    instruction->replaceAllUsesWith(tmp1);
    instruction->eraseFromParent();

    if (error_bb) {
      BasicBlock *before = state->getParent();
      BasicBlock *after = before->splitBasicBlock(last->getNextNode(), "after");

      auto old_terminator = before->getTerminator();
      IRBuilder<> Builder(old_terminator);

      auto *fault_detected_cond =
          Builder.CreateTrunc(state, builder.getInt1Ty(), "", true);

      auto ProfMD = create_br_weights(state->getContext(), 1, 2000);
      Builder.CreateCondBr(fault_detected_cond, error_bb, after, ProfMD);
      old_terminator->eraseFromParent();
    }
  }
}

SmallVector<llvm::Instruction *, 1> find_values_to_harden(Function &function) {
  SmallVector<llvm::Instruction *, 1> opaque_calls;
  for (auto &basic_block : function)
    for (auto &instruction : basic_block) {
      if (auto *call = dyn_cast<CallInst>(&instruction)) {
        if (auto *called_op = call->getCalledOperand()) {
          if (called_op->getName().starts_with("internal_cfip_opaque_call_"))
            opaque_calls.push_back(&instruction);
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

    auto *ProfMD = create_br_weights(Ctx, 1, 2000);
    Builder.CreateCondBr(fault_detected_cond, error_bb, new_ret_bb, ProfMD);
    ret_bb->getTerminator()->eraseFromParent();
  }
}
llvm::Instruction *unwrap_call(llvm::Instruction *opaque_call,
                               IRBuilder<> &alloca_builder) {
  // rustc attribute hides value behind a call to prevent over-optimizations
  // it's no longer needed thus we unwrap them:
  // before: critical_var = opaque_call(start_value)
  // after: critical_var = start_value
  llvm::IRBuilder<> value_unwrapper_builder(opaque_call);

  auto *opaque_value_ptr = alloca_builder.CreateAlloca(
      opaque_call->getType(), nullptr, "unwrapped_value");
  [[maybe_unused]] auto store_critical_value =
      value_unwrapper_builder.CreateStore(opaque_call->getOperand(0),
                                          opaque_value_ptr);

  llvm::LoadInst *critical_value_use = value_unwrapper_builder.CreateLoad(
      opaque_call->getType(), opaque_value_ptr, false, "replacement");

  opaque_call->replaceAllUsesWith(critical_value_use);
  opaque_call->eraseFromParent();
  return critical_value_use;
}

llvm::Instruction *insert_bool(IRBuilder<> &alloca_builder, LLVMContext &Ctx) {

  llvm::Type *i8_type = llvm::IntegerType::getInt8Ty(Ctx);

  // create value to store state of program
  llvm::Instruction *fault_detected_ptr =
      alloca_builder.CreateAlloca(i8_type, nullptr, "fault_detected");
  alloca_builder.CreateStore(llvm::ConstantInt::get(i8_type, false),
                             fault_detected_ptr);

  alloca_builder.SetInsertPoint(fault_detected_ptr); // reset insert point
  return fault_detected_ptr;
}

size_t harden_chosen(Function &function, bool atomic_state, bool check_asap) {
  LLVMContext &Ctx = function.getContext();

  // In 99.9% cases there should be no more than one
  SmallVector<llvm::Instruction *, 1> opaque_calls =
      find_values_to_harden(function);

  if (opaque_calls.empty())
    return 0;

  llvm::Instruction *first_instruction = &*inst_begin(function);
  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> alloca_builder(first_instruction);

  auto fault_detected_ptr = insert_bool(alloca_builder, Ctx);

  DenseSet<Value *> users_of_critical_values;
  for (auto &opaque_call : opaque_calls) {
    if (opaque_call->use_empty()) {
      errs() << "WARNING: nothing to harden, value not used\n";
      continue;
    }

    auto *critical_value_use = unwrap_call(opaque_call, alloca_builder);

    // now that the value is extracted we find all it's users
    // but all users of my users are also my users (recursively)
    getUsersRec(critical_value_use, users_of_critical_values);
  }

  auto *error_bb = insert_error_bb(function);
  // call function to finalize the protection by adding a check before any
  // return
  add_integrity_check(function, fault_detected_ptr, atomic_state, error_bb);

  // add redundancy to all users of critical values
  for (auto *user : users_of_critical_values) {
    add_redundancy(fault_detected_ptr, user, atomic_state,
                   check_asap ? error_bb : nullptr);
  }

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
  F.removeFnAttr(Attribute::OptimizeForSize);
  F.removeFnAttr(Attribute::MinSize);
  F.removeFnAttr(Attribute::AlwaysInline);
}

template <auto Fn> struct Cfip : PassInfoMixin<Cfip<Fn>> {
  const bool atomic_state;
  const bool check_asap;
  Cfip(bool atomic_state_ = false, bool check_asap_ = false)
      : atomic_state(atomic_state_), check_asap{check_asap_} {}

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    FunctionAnalysisManager *FAM =
        &AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
    DenseSet<Function *> hardened_functions;

    for (auto &F : M) {
      if (Fn(F, atomic_state, check_asap)) {
        finalize(F, *FAM);
        hardened_functions.insert(&F);
      }
    }

    if (hardened_functions.empty()) {
      return PreservedAnalyses::none();
    } else {
      return PreservedAnalyses::all();
    }
  }

  static bool isRequired() { return true; }
};

size_t harden_all(Function &function, bool atomic_state, bool check_asap) {
  LLVMContext &Ctx = function.getContext();
  auto inst_cnt = function.getInstructionCount();
  if (!inst_cnt)
    return 0;

  llvm::Instruction *first_instruction = &*inst_begin(function);
  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> alloca_builder(first_instruction);

  auto *fault_detected_ptr = insert_bool(alloca_builder, Ctx);
  auto *error_bb = insert_error_bb(function);
  // add redundancy to all
  std::vector<llvm::Instruction *> instructions(inst_cnt);
  for (inst_iterator inst = inst_begin(function), end = inst_end(function);
       inst != end; ++inst) {
    instructions.push_back(&*inst);
  }

  add_integrity_check(function, fault_detected_ptr, atomic_state, error_bb);

  for (auto *i : instructions) {
    if (i) {
      add_redundancy(fault_detected_ptr, i, atomic_state,
                     check_asap ? error_bb : nullptr);
    }
  }

  return 1;
}

} // namespace

struct CfipOpts {
  bool atomic_state;
  bool harden_all;
  bool check_asap;
  CfipOpts(bool atomic_state_ = false, bool harden_all_ = false,
           bool check_asap_ = false)
      : atomic_state(atomic_state_), harden_all(harden_all_),
        check_asap(check_asap_) {}
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
    else
      return make_error<StringError>("Unknown parameter: " + ParamName,
                                     inconvertibleErrorCode());
  }
  return Result;
}
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
                                                   Params->check_asap});
                    } else {
                      MPM.addPass(Cfip<harden_chosen>{Params->atomic_state,
                                                      Params->check_asap});
                    }
                    errs() << "CFIP: " << Params->harden_all << " "
                           << Params->atomic_state << "\n";
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
