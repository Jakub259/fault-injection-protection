#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <utility>
using namespace llvm;

namespace {

struct Firv2 : PassInfoMixin<Firv2> {
  void build_function(Function *function, Function *original_function,
                      Function *cmp_function) {
    LLVMContext &Ctx = function->getContext();
    original_function->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
    BasicBlock *block = BasicBlock::Create(Ctx, "entry", function);
    IRBuilder<> builder(block);

    // Check if the function has an sret attribute
    // opt says:
    // * Cannot have multiple 'sret' parameters!
    // * Attribute 'sret' is not on first or second parameter!
    Type *sretType;
    unsigned sretPos;
    for (auto i : {0u, 1u}) {
      if ((sretType = original_function->getParamStructRetType(i))) {
        sretPos = i;
        break;
      }
    }
    SmallVector<Value *> args_vec;
    for (auto &arg : function->args()) {
      args_vec.push_back(&arg);
    }

    CallInst *cmp = nullptr;
    Value *return_value;
    if (sretType) {
      // For sret functions, we pass the original sret pointer to the first call
      // and only create a temporary alloca for the second call
      Value *sretAlloca2 = builder.CreateAlloca(sretType);

      SmallVector<Value *> args_vec_temp(args_vec);

      // Replace the sret argument for the second call with our temporary alloca
      args_vec_temp[sretPos] = sretAlloca2;

      auto call1 = builder.CreateCall(original_function, args_vec);
      call1->setCallingConv(original_function->getCallingConv());

      auto call2 = builder.CreateCall(original_function, args_vec_temp);
      call2->setCallingConv(original_function->getCallingConv());

      cmp = builder.CreateCall(cmp_function, {args_vec[sretPos], sretAlloca2});
      cmp->setCallingConv(cmp_function->getCallingConv());
      return_value = call1;
    } else {
      SmallVector<Value *> args_vec;
      for (auto &arg : original_function->args()) {
        args_vec.push_back(&arg);
      }

      auto return_type = original_function->getType();

      auto return_var_1 = builder.CreateAlloca(return_type);
      auto return_var_2 = builder.CreateAlloca(return_type);

      auto call1 = builder.CreateCall(original_function, args_vec);
      call1->setCallingConv(original_function->getCallingConv());

      auto call2 = builder.CreateCall(original_function, args_vec);
      call2->setCallingConv(original_function->getCallingConv());

      builder.CreateStore(call1, return_var_1);
      builder.CreateStore(call2, return_var_2);

      cmp = builder.CreateCall(cmp_function, {return_var_1, return_var_2});
      cmp->setCallingConv(cmp_function->getCallingConv());
      return_value = call1;
    }
    cmp_function->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
    cmp_function->addFnAttr(Attribute::InlineHint);

    BasicBlock *return_block =
        BasicBlock::Create(function->getContext(), "return", function);
    BasicBlock *error_block =
        BasicBlock::Create(function->getContext(), "error", function);

    builder.CreateCondBr(cmp, return_block, error_block,
                         MDBuilder(Ctx).createBranchWeights(1, 2000));

    IRBuilder<> return_builder(return_block);
    if (return_value->getType()->isVoidTy()) {
      return_builder.CreateRetVoid();
    } else {
      return_builder.CreateRet(return_value);
    }

    IRBuilder<> error_builder(error_block);
    error_builder.CreateCall(
        Intrinsic::getDeclaration(function->getParent(), Intrinsic::trap));
    error_builder.CreateUnreachable();
  }
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) {
    SmallVector<std::pair<Value *, StringRef>> hardening_requests;

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
                  hardening_requests.push_back({call, called_name});
                  errs() << "Found function to harden: " << function.getName()
                         << "\n";
                }
              }
            }
          }
      }
    }
    for (auto &[hardening_request, IDX] : hardening_requests) {
      auto call = cast<CallBase>(hardening_request);
      auto function = call->getCaller();
      auto original_name = function->getName();
      if (function->getType()->isVoidTy()) {
        errs() << "Skipping void function " << original_name << "\n";
        continue;
      }
      auto new_name = "original." + original_name;
      function->setName(new_name);
      auto new_function = cast<Function>(
          M.getOrInsertFunction(original_name, function->getFunctionType())
              .getCallee());

      auto cmp_name = ("internal_firv2_" + IDX + "_eq").str();
      auto cmp_func = cast<Function>(M.getFunction(cmp_name));

      function->replaceAllUsesWith(new_function);
      build_function(new_function, function, cmp_func);
      call->eraseFromParent();
    }
    if (hardening_requests.empty()) {
      return PreservedAnalyses::all();
    } else {
      return PreservedAnalyses::none();
    }
  }

  static bool isRequired() { return true; }
};
} // namespace

llvm::PassPluginLibraryInfo getFirv2PluginInfo() {
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
  return getFirv2PluginInfo();
}
