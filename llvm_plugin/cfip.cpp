
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include <functional>

using namespace llvm;
#define DEBUG_TYPE "cfip"

namespace {


size_t harden_fn(Function &function) {
  [[maybe_unused]] LLVMContext &Ctx = function.getContext();
  errs() << __func__ << "\nHello " << function.getName() << "\n";
  return 0;

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
                    FPM.addPass(Cfip());
                  }
                  return false;
                });
          }};
}
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getCfipPluginInfo();
}
