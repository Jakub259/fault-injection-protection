#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Scalar/Reg2Mem.h"

using namespace llvm;
#define DEBUG_TYPE "cfip"

namespace {

void getUsersRec(Value *const U, SetVector<Value *> &OutSV) {
  OutSV.insert(U);
  for (auto *user : U->users()) {
    getUsersRec(user, OutSV);
  }
}

size_t harden_fn(Function &function) {
  [[maybe_unused]] LLVMContext &Ctx = function.getContext();

  // In 99.9% cases there should be no more than one
  SmallVector<llvm::Instruction *, 1> opaque_calls;

  // find values to harden
  for (auto &basic_block : function)
    for (auto &instruction : basic_block) {
      if (auto *call = dyn_cast<CallInst>(&instruction)) {
        if (auto *called_op = call->getCalledOperand()) {
          if (called_op->getName().starts_with("internal_cfip_opaque_call_"))
            opaque_calls.push_back(&instruction);
        }
      }
    }

  for (auto &opaque_call : opaque_calls) {
    // rustc attribute hides value behind a call to prevent over-optimizations
    // it's no longer needed thus we unwrap them:
    // before: critical_var = opaque_call(start_value)
    // after: critical_var = start_value
    llvm::IRBuilder<> builder(opaque_call);
    // create space for the value
    auto *opaque_value_alloca =
        builder.CreateAlloca(opaque_call->getType(), nullptr, "result");
    [[maybe_unused]] auto store_critical_value =
        builder.CreateStore(opaque_call->getOperand(0), opaque_value_alloca);

    llvm::LoadInst *first_critical_value_use = builder.CreateLoad(
        opaque_call->getType(), opaque_value_alloca, false, "replacement");

    if (!opaque_call->use_empty()) {
      opaque_call->replaceAllUsesWith(first_critical_value_use);
      opaque_call->eraseFromParent();
    }

    // now that the value is extracted we find all it's users
    // but all users of my users are also my users (recursively)
    SetVector<Value *> ALLUsers{};
    getUsersRec(first_critical_value_use, ALLUsers);

  }

  return opaque_calls.size();
}
struct Cfip : PassInfoMixin<Cfip> {
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &) {
    if (harden_fn(F))
      return PreservedAnalyses::none();
    else
      return PreservedAnalyses::all();
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
                    FPM.addPass(RegToMemPass());
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
