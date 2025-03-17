#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
using namespace llvm;

namespace {

struct Firv2 : PassInfoMixin<Firv2> {
  void build_function(Function *function, Function *original_function) {
    LLVMContext &Ctx = function->getContext();
    original_function->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
    original_function->addFnAttr(Attribute::NoInline);
    original_function->setCallingConv(CallingConv::Fast);
    errs() << "Building function " << *function << "\n";
    BasicBlock *block = BasicBlock::Create(Ctx, "entry", function);
    IRBuilder<> builder(block);
    SmallVector<Value *> args_vec;
    for (auto &arg : function->args()) {
      args_vec.push_back(&arg);
    }
    auto call1 = builder.CreateCall(original_function, {args_vec});
    auto call2 = builder.CreateCall(original_function, {args_vec});
    auto cmp = builder.CreateCmp(CmpInst::Predicate::ICMP_EQ, call1, call2);
    BasicBlock *return_block =
        BasicBlock::Create(function->getContext(), "return", function);
    BasicBlock *error_block =
        BasicBlock::Create(function->getContext(), "error", function);
    builder.CreateCondBr(cmp, return_block, error_block,
                         MDBuilder(Ctx).createBranchWeights(1, 2000));

    IRBuilder<> return_builder(return_block);
    return_builder.CreateRet(call1);

    IRBuilder<> error_builder(error_block);
    error_builder.CreateCall(
        Intrinsic::getDeclaration(function->getParent(), Intrinsic::trap));
    error_builder.CreateUnreachable();
  }
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    SmallVector<Function *> functions;
    for (auto &function : M) {
      functions.push_back(&function);
    }
    for (auto &function : functions) {
      auto original_name = function->getName();
      auto new_name = "harden." + original_name;
      function->setName(new_name);
      auto new_function =
          M.getOrInsertFunction(original_name, function->getFunctionType());
      build_function(cast<Function>(new_function.getCallee()), function);
    }
    if (not functions.empty()) {
      return PreservedAnalyses::none();
    } else {
      return PreservedAnalyses::all();
    }
  }

  static bool isRequired() { return true; }
};
} // namespace

llvm::PassPluginLibraryInfo getCfipPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "firv2", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "firv2") {
                    MPM.addPass(Firv2{});
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
