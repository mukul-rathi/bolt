#include <iostream>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

llvm::FunctionType *IRCodegenVisitor::codegenFunctionType(
    const FunctionIR &function) {
  std::vector<llvm::Type *> paramTypes;
  for (auto &param : function.params) {
    paramTypes.push_back(param->paramType->accept(*this));
  }
  llvm::Type *returnType = function.returnType->accept(*this);
  return llvm::FunctionType::get(returnType, paramTypes, false /* isVarArgs */
  );
}

void IRCodegenVisitor::codegenFunction(const FunctionIR &function) {
  llvm::FunctionType *functionType = codegenFunctionType(function);
  llvm::Function *llvmFun =
      llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                             function.functionName, module.get());
  llvm::BasicBlock *entryBasicBlock =
      llvm::BasicBlock::Create(*context, "entry", llvmFun);
  builder->SetInsertPoint(entryBasicBlock);

  // initialise var env with function params
  varEnv.clear();
  for (auto &param : llvmFun->args()) {
    int paramNo = param.getArgNo();
    std::string paramName = function.params[paramNo]->paramName;
    llvm::Type *paramType = llvmFun->getFunctionType()->getParamType(paramNo);
    varEnv[paramName] =
        builder->CreateAlloca(paramType, nullptr, llvm::Twine(paramName));
    builder->CreateStore(&param, varEnv[paramName]);
  }

  // gen code for body of function
  llvm::Value *returnValue;  // this is the value of the last expr in the body
  for (auto &expr : function.bodyExpr) {
    returnValue = expr->accept(*this);
  }
  // create a return instruction from last expression
  if (llvmFun->getReturnType()->isVoidTy()) {
    builder->CreateRetVoid();
  } else {
    builder->CreateRet(returnValue);
  }

  llvm::verifyFunction(*llvmFun);
}

void IRCodegenVisitor::codegenFunctions(
    const std::vector<std::unique_ptr<FunctionIR>> &functions) {
  for (auto &function : functions) {
    codegenFunction(*function);
  }
}