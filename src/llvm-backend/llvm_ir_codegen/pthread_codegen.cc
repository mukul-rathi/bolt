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
  std::vector<llvm::AllocaInst *> freeVarList;
  for (auto &var : asyncExpr.freeVars) {
    freeVarList.push_back(varEnv[var]);
  }

  // create async function and argument
  llvm::StructType *argStructTy = codegenAsyncFunArgStructType(freeVarList);
  llvm::Value *argStruct = codegenAsyncFunArgStruct(asyncExpr, argStructTy);
  llvm::Function *asyncFun = codegenAsyncFunction(asyncExpr, argStructTy);

  // restore current state
  builder->SetInsertPoint(currentBB);
  for (int i = 0; i < asyncExpr.freeVars.size(); i++) {
    varEnv[asyncExpr.freeVars[i]] = freeVarList[i];
  }

  // spawn thread
  llvm::Function *pthread_create =
      module->getFunction(llvm::StringRef("pthread_create"));
  llvm::Value *voidPtrNull = llvm::Constant::getNullValue(
      llvm::Type::getInt8Ty(*context)->getPointerTo());
  llvm::Value *args[4] = {
      pthread,
      voidPtrNull,
      asyncFun,
      builder->CreatePointerCast(argStruct, voidPtrTy),
  };
  builder->CreateCall(pthread_create, args);
}

llvm::Function *IRCodegenVisitor::codegenAsyncFunction(
    const AsyncExprIR &asyncExpr, llvm::StructType *argStructTy) {
  // find unique function name (_async 0, _async1, _async2 etc)
  int fnIndex = 0;
  while (module->getFunction("_async" + std::to_string(fnIndex))) {
    fnIndex++;
  }
  std::string functionName = "_async" + std::to_string(fnIndex);

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

  // cast (void *) arg back to original arg type
  llvm::Value *argVoidPtr = asyncFun->args().begin();
  llvm::Value *argStructPtr =
      builder->CreatePointerCast(argVoidPtr, argStructTy->getPointerTo());

  // allocate function args on the stack
  for (int i = 0; i < asyncExpr.freeVars.size(); i++) {
    llvm::Value *freeVarVal = builder->CreateLoad(
        builder->CreateStructGEP(argStructTy, argStructPtr, i));
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

llvm::StructType *IRCodegenVisitor::codegenAsyncFunArgStructType(
    const std::vector<llvm::AllocaInst *> &freeVarList) {
  llvm::StructType *structPtrArgType =
      llvm::StructType::create(*context, "structPtrArgType");
  std::vector<llvm::Type *> freeVarsTypes;
  for (auto &var : freeVarList) {
    freeVarsTypes.push_back(var->getAllocatedType());
  }
  structPtrArgType->setBody(llvm::ArrayRef<llvm::Type *>(freeVarsTypes));

  return structPtrArgType;
}

llvm::Value *IRCodegenVisitor::codegenAsyncFunArgStruct(
    const AsyncExprIR &asyncExpr, llvm::StructType *structPtrArgType) {
  // create a struct containing the values of all the free variables
  llvm::AllocaInst *argStruct =
      builder->CreateAlloca(structPtrArgType, nullptr);
  for (int i = 0; i < asyncExpr.freeVars.size(); i++) {
    llvm::Value *freeVarVal =
        builder->CreateLoad(varEnv[asyncExpr.freeVars[i]]);

    llvm::Value *field =
        builder->CreateStructGEP(structPtrArgType, argStruct, i);
    builder->CreateStore(freeVarVal, field);
  }
  return argStruct;
}
