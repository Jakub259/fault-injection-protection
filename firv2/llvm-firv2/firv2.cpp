#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
using namespace llvm;

namespace {

struct Firv2 : PassInfoMixin<Firv2> {
  void build_function(Function *function, Function *original_function,
                      Function *cmp_function) {
    LLVMContext &Ctx = function->getContext();
    original_function->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
    original_function->addFnAttr(Attribute::NoInline);
    original_function->setCallingConv(CallingConv::Fast);
    BasicBlock *block = BasicBlock::Create(Ctx, "entry", function);
    IRBuilder<> builder(block);
    SmallVector<Value *> args_vec;
    for (auto &arg : original_function->args()) {
      args_vec.push_back(&arg);
    }

    auto return_type = original_function->getReturnType();
    auto stack_var_1 = builder.CreateAlloca(return_type);
    auto stack_var_2 = builder.CreateAlloca(return_type);
    auto call1 = builder.CreateCall(original_function, {args_vec});
    auto call2 = builder.CreateCall(original_function, {args_vec});
    builder.CreateStore(call1, stack_var_1);
    builder.CreateStore(call2, stack_var_2);
    auto cmp = builder.CreateCall(cmp_function, {stack_var_1, stack_var_2});
    errs() << "Function: " << *function << "\n";

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
    SmallVector<Value *> hardening_requests;
    SmallVector<StringRef> IDXs;

    for (auto &function : M) {
      if (function.isDeclaration()) {
        continue;
      }
      for (auto &bb : function) {
        for (auto &inst : bb)
          if (auto call = dyn_cast<CallBase>(&inst)) {
            if (auto called_function = call->getCalledFunction()) {
              {
                auto called_name = called_function->getName();
                if (called_name.consume_front("internal_firv2_") and
                    called_name.consume_back("_identifier")) {
                  hardening_requests.push_back(call);
                  IDXs.push_back(called_name);
                  errs() << "Found function to harden: " << function.getName()
                         << "\n";
                }
              }
            }
          }
      }
    }
    for (size_t i = 0; i < hardening_requests.size(); ++i) {
      auto call = cast<CallBase>(hardening_requests[i]);
      auto function = call->getCaller();
      if (function->getType()->isVoidTy()) {
        errs() << "Skipping void function " << function->getName() << "\n";
        continue;
      }
      auto original_name = function->getName();
      auto new_name = "original." + original_name;
      function->setName(new_name);
      auto new_function = cast<Function>(
          M.getOrInsertFunction(original_name, function->getFunctionType())
              .getCallee());

      auto cmp_name = ("internal_firv2_" + IDXs[i] + "_eq").str();
      auto cmp_func =
          cast<Function>(M.getOrInsertFunction(cmp_name, nullptr).getCallee());

      build_function(new_function, function, cmp_func);
      call->eraseFromParent();
    }
    if (not hardening_requests.empty()) {
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
