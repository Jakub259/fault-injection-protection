#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
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

void add_redundancy(llvm::Value *fault_detected_ptr, llvm::Value *value) {
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

    // fault_detected |= local_fault_detected
    auto fault_detected =
        builder.CreateLoad(builder.getInt1Ty(), fault_detected_ptr, false);

    // if it's vector we need to reduce it to scalar
    if (local_fault_detected->getType()->isVectorTy()) {
      local_fault_detected = builder.CreateOrReduce(local_fault_detected);
    }

    Value *updated_fault_detection_state =
        builder.CreateOr(fault_detected, local_fault_detected);

    builder.CreateStore(updated_fault_detection_state, fault_detected_ptr);

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

void add_integrity_check(Function &function, Instruction *fault_detected_ptr) {
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
    auto fault_detected =
        Builder.CreateLoad(Builder.getInt1Ty(), fault_detected_ptr, false);

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

    // replace terminator with conditional branch
    Builder.CreateCondBr(fault_detected, error_bb, new_ret_bb, ProfMD);
    ret_bb->getTerminator()->eraseFromParent();
  }
}
llvm::Instruction *unwrap_call(llvm::Instruction *opaque_call,
                               IRBuilder<> &builder) {
  // rustc attribute hides value behind a call to prevent over-optimizations
  // it's no longer needed thus we unwrap them:
  // before: critical_var = opaque_call(start_value)
  // after: critical_var = start_value
  llvm::IRBuilder<> value_unwrapper_builder(opaque_call);

  auto *opaque_value_ptr =
      builder.CreateAlloca(opaque_call->getType(), nullptr, "result");
  [[maybe_unused]] auto store_critical_value =
      value_unwrapper_builder.CreateStore(opaque_call->getOperand(0),
                                          opaque_value_ptr);

  llvm::LoadInst *critical_value_use = value_unwrapper_builder.CreateLoad(
      opaque_call->getType(), opaque_value_ptr, false, "replacement");

  opaque_call->replaceAllUsesWith(critical_value_use);
  opaque_call->eraseFromParent();
  return critical_value_use;
}
size_t harden_fn(Function &function) {
  LLVMContext &Ctx = function.getContext();

  // In 99.9% cases there should be no more than one
  SmallVector<llvm::Instruction *, 1> opaque_calls =
      find_values_to_harden(function);

  if (opaque_calls.empty())
    return 0;

  llvm::Instruction *first_instruction = &*inst_begin(function);
  // note: the builder is used to create alloca
  // it should point to begin of entry block
  llvm::IRBuilder<> builder(first_instruction);

  llvm::Type *i1_type = llvm::IntegerType::getInt1Ty(Ctx);

  // create value to store state of program
  llvm::Instruction *fault_detected_ptr =
      builder.CreateAlloca(i1_type, nullptr, "fault_detected");
  builder.CreateStore(llvm::ConstantInt::get(i1_type, false),
                      fault_detected_ptr);
  builder.SetInsertPoint(
      first_instruction); // there are more allocas coming soon

  DenseSet<Value *> users_of_critical_values;
  for (auto &opaque_call : opaque_calls) {
    if (opaque_call->use_empty()) {
      errs() << "WARNING: nothing to harden, value not used\n";
      continue;
    }

    auto *critical_value_use = unwrap_call(opaque_call, builder);

    // now that the value is extracted we find all it's users
    // but all users of my users are also my users (recursively)
    getUsersRec(critical_value_use, users_of_critical_values);
  }
  // add redundancy to all users of critical values
  for (auto *user : users_of_critical_values) {
    add_redundancy(fault_detected_ptr, user);
  }

  // call function to finalize the protection by adding a check before any
  // return
  add_integrity_check(function, fault_detected_ptr);

  return opaque_calls.size();
}

struct Cfip : PassInfoMixin<Cfip> {
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) {
    if (harden_fn(F)) {
      // cleanup
      SROAPass(SROAOptions::ModifyCFG).run(F, AM);
      DCEPass().run(F, AM);

      // otherwise our hardening will be optimized out
      F.addFnAttr(Attribute::OptimizeNone);
      // required be OptimizeNone
      F.addFnAttr(Attribute::NoInline);
      F.removeFnAttr(Attribute::OptimizeForSize);
      F.removeFnAttr(Attribute::MinSize);
      return PreservedAnalyses::none();
    } else {
      return PreservedAnalyses::all();
    }
  }

  static bool isRequired() { return true; }
};

} // namespace

llvm::PassPluginLibraryInfo getCfipPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "cfip", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, FunctionPassManager &FPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "cfip") {
                    FPM.addPass(Cfip());
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
