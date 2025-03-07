#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
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

void add_redundancy(llvm::Value *fault_detected_ptr, llvm::Value *value,
                    bool thread_safe) {
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

    if (thread_safe) {
      local_fault_detected =
          builder.CreateZExt(local_fault_detected, builder.getInt8Ty());
      builder.CreateAtomicRMW(AtomicRMWInst::BinOp::Or, fault_detected_ptr,
                              local_fault_detected, MaybeAlign(1),
                              AtomicOrdering::AcquireRelease);
    } else {
      // fault_detected |= local_fault_detected
      auto fault_detected =
          builder.CreateLoad(builder.getInt8Ty(), fault_detected_ptr, false);

      local_fault_detected =
          builder.CreateZExt(local_fault_detected, builder.getInt8Ty());

      Value *updated_fault_detection_state =
          builder.CreateOr(fault_detected, local_fault_detected);

      builder.CreateStore(updated_fault_detection_state, fault_detected_ptr);
    }

    // TODO: Does it matter if we replace it with tmp1 or tmp2 or pick random?
    instruction->replaceAllUsesWith(tmp1);
    instruction->eraseFromParent();
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

void add_integrity_check(Function &function, Instruction *fault_detected_ptr,
                         bool thread_safe) {
  auto &Ctx = function.getContext();

  // add a error handling basic block
  BasicBlock *error_bb = BasicBlock::Create(Ctx, "error_handling", &function);
  {
    IRBuilder<> builder(error_bb);
    builder.CreateCall(
        Intrinsic::getDeclaration(function.getParent(), Intrinsic::trap));
    builder.CreateUnreachable();
  }

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

    MDBuilder MDBuilder(Ctx);
    Metadata *branch_weights[] = {
        MDBuilder.createString("branch_weights"),
        MDBuilder.createString("expected"),
        ValueAsMetadata::get(
            ConstantInt::get(Type::getInt32Ty(Ctx), 1)), // cold(error) path
        ValueAsMetadata::get(
            ConstantInt::get(Type::getInt32Ty(Ctx), 2000)), // hot(success) path
    };
    MDNode *ProfMD = MDNode::get(Ctx, branch_weights);

    auto *fault_detected =
        Builder.CreateLoad(Builder.getInt8Ty(), fault_detected_ptr, false);
    if (thread_safe) {
      fault_detected->setAtomic(AtomicOrdering::Acquire);
    }

    // nuw keyword is present, and any of the truncated bits are non-zero, the
    // result is a poison value. we use i8 as boolean(i1) so it's safe to
    // truncate with nuw
    auto *fault_detected_cond =
        Builder.CreateTrunc(fault_detected, Builder.getInt1Ty(), "", true);
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

  auto *opaque_value_ptr =
      alloca_builder.CreateAlloca(opaque_call->getType(), nullptr, "result");
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

size_t harden_chosen(Function &function, bool thread_safe) {
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
  // add redundancy to all users of critical values
  for (auto *user : users_of_critical_values) {
    add_redundancy(fault_detected_ptr, user, thread_safe);
  }

  // call function to finalize the protection by adding a check before any
  // return
  add_integrity_check(function, fault_detected_ptr, thread_safe);

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
  const bool thread_safe;
  Cfip(bool thread_safe = false) : thread_safe(thread_safe) {}

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    FunctionAnalysisManager *FAM =
        &AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
    DenseSet<Function *> hardened_functions;

    for (auto &F : M) {
      if (Fn(F, thread_safe)) {
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

size_t harden_all(Function &function, bool thread_safe) {
  LLVMContext &Ctx = function.getContext();
  auto inst_cnt = function.getInstructionCount();
  if (!inst_cnt)
    return 0;

  llvm::Instruction *first_instruction = &*inst_begin(function);
  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> alloca_builder(first_instruction);

  auto *fault_detected_ptr = insert_bool(alloca_builder, Ctx);

  // add redundancy to all
  std::vector<llvm::Instruction *> instructions(inst_cnt);
  for (inst_iterator inst = inst_begin(function), end = inst_end(function);
       inst != end; ++inst) {
    instructions.push_back(&*inst);
  }
  for (auto *i : instructions) {
    if (i) {
      add_redundancy(fault_detected_ptr, i, thread_safe);
    }
  }
  add_integrity_check(function, fault_detected_ptr, thread_safe);

  return 1;
}

} // namespace

llvm::PassPluginLibraryInfo getCfipPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "cfip", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "cfip") {
                    MPM.addPass(Cfip<harden_chosen>());
                    return true;
                  }
                  if (Name == "acfip") {
                    MPM.addPass(Cfip<harden_all>());
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
