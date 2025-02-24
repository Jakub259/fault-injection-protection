#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Scalar/Reg2Mem.h"

using namespace llvm;
#define DEBUG_TYPE "cfip"

namespace {


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
