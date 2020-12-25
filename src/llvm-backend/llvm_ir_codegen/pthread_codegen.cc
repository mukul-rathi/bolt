#include <string>

#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/ThreadPool.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

void IRCodegenVisitor::codegenJoinPThreads(
    const std::vector<llvm::Value *> pthreadPtrs) {
  llvm::Function *pthread_join =
      module->getFunction(llvm::StringRef("pthread_join"));
  llvm::Type *voidPtrPtrTy =
      llvm::Type::getInt8Ty(*context)->getPointerTo()->getPointerTo();
  for (auto &pthreadPtr : pthreadPtrs) {
    llvm::Value *pthread = builder->CreateLoad(pthreadPtr);
    builder->CreateCall(pthread_join,
                        {pthread, llvm::Constant::getNullValue(voidPtrPtrTy)});
  }
}

void IRCodegenVisitor::codegenCreatePThread(llvm::Value *pthread,
                                            const AsyncExprIR &asyncExpr) {
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();

  // save current state
  llvm::BasicBlock *currentBB = builder->GetInsertBlock();
  std::vector<llvm::AllocaInst *> originalVarMap;
  for (auto &var : asyncExpr.freeVars) {
    originalVarMap.push_back(varEnv[var]);
  }

  // create async function argument type
  llvm::StructType *functionArgType =
      llvm::StructType::create(*context, "function_arg_type");
  std::vector<llvm::Type *> freeVarsTypes;
  for (auto &var : originalVarMap) {
    freeVarsTypes.push_back(var->getAllocatedType());
  }
  functionArgType->setBody(llvm::ArrayRef<llvm::Type *>(freeVarsTypes));

  llvm::Value *asyncFunArg =
      codegenAsyncFunctionArg(asyncExpr, functionArgType);

  llvm::Function *asyncFun =
      codegenAsyncFunction(asyncExpr, functionArgType, asyncFunArg->getType());

  // restore current state
  for (int i = 0; i < asyncExpr.freeVars.size(); i++) {
    varEnv[asyncExpr.freeVars[i]] = originalVarMap[i];
  }
  builder->SetInsertPoint(currentBB);

  // spawn thread
  llvm::Function *pthread_create =
      module->getFunction(llvm::StringRef("pthread_create"));
  llvm::Value *voidPtrNull = llvm::Constant::getNullValue(
      llvm::Type::getInt8Ty(*context)->getPointerTo());
  llvm::Value *args[4] = {
      pthread,
      voidPtrNull,
      asyncFun,
      builder->CreatePointerCast(asyncFunArg, voidPtrTy),
  };
  builder->CreateCall(pthread_create, args);
}

llvm::Function *IRCodegenVisitor::codegenAsyncFunction(
    const AsyncExprIR &asyncExpr, llvm::StructType *functionArgType,
    llvm::Type *functionArgPointerType) {
  // find unique function name (_async 0, async_1, async_2 etc)
  int threadIndex = 0;
  while (module->getFunction("_async" + std::to_string(threadIndex))) {
    threadIndex++;
  }
  std::string functionName = "_async" + std::to_string(threadIndex);

  // define function type to match what pthread_create expects

  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();
  llvm::FunctionType *asyncFunType = llvm::FunctionType::get(
      voidPtrTy, llvm::ArrayRef<llvm::Type *>({voidPtrTy}),
      /* has variadic args */ false);

  llvm::Function *asyncFun =
      llvm::Function::Create(asyncFunType, llvm::Function::ExternalLinkage,
                             functionName, module.get());

  // define body of function

  llvm::BasicBlock *entryBasicBlock =
      llvm::BasicBlock::Create(*context, "entry", asyncFun);
  builder->SetInsertPoint(entryBasicBlock);

  // cast void * arg back to original arg type
  llvm::Value *voidPtrArg = asyncFun->args().begin();
  llvm::Value *arg =
      builder->CreatePointerCast(voidPtrArg, functionArgPointerType);

  // update map with function args
  for (int i = 0; i < asyncExpr.freeVars.size(); i++) {
    llvm::Value *freeVarVal =
        builder->CreateLoad(builder->CreateStructGEP(functionArgType, arg, i));
    varEnv[asyncExpr.freeVars[i]] = builder->CreateAlloca(
        freeVarVal->getType(), nullptr, llvm::Twine(asyncExpr.freeVars[i]));
    builder->CreateStore(freeVarVal, varEnv[asyncExpr.freeVars[i]]);
  }

  // generate IR for body of function

  for (auto &expr : asyncExpr.exprs) {
    expr->codegen(*this);
  }
  builder->CreateRet(llvm::Constant::getNullValue(voidPtrTy));
  llvm::verifyFunction(*asyncFun);

  return asyncFun;
}

llvm::Value *IRCodegenVisitor::codegenAsyncFunctionArg(
    const AsyncExprIR &asyncExpr, llvm::StructType *functionArgType) {
  // create a struct containing the values of all the free variables

  llvm::AllocaInst *arg = builder->CreateAlloca(functionArgType, nullptr);
  for (int i = 0; i < asyncExpr.freeVars.size(); i++) {
    llvm::Value *freeVarVal =
        builder->CreateLoad(varEnv[asyncExpr.freeVars[i]]);

    llvm::Value *field = builder->CreateStructGEP(functionArgType, arg, i);
    builder->CreateStore(freeVarVal, field);
  }
  return arg;
}
