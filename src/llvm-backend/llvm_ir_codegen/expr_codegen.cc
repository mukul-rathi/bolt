
#include <iostream>
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
llvm::Value *IRCodegenVisitor::codegen(const IdentifierVarIR &var) {
  return varEnv[var.varName];
};
llvm::Value *IRCodegenVisitor::codegen(const IdentifierObjFieldIR &objField) {
  llvm::AllocaInst *objPtr =
      varEnv[objField.objName];  // pointer to value of variable stack

  return builder->CreateStructGEP(
      objPtr->getAllocatedType()
          ->getPointerElementType() /* get type of element on heap*/,
      builder->CreateLoad(objPtr) /*get heap ptr */, objField.fieldIndex);
};

llvm::Value *IRCodegenVisitor::codegen(const ExprIntegerIR &expr) {
  return llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)),
                                      expr.val);
  ;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprBooleanIR &expr) {
  return llvm::ConstantInt::getSigned((llvm::Type::getInt1Ty(*context)),
                                      expr.val);
};
llvm::Value *IRCodegenVisitor::codegen(const ExprIdentifierIR &expr) {
  llvm::Value *id = expr.identifier->accept(*this);
  return builder->CreateLoad(id);
};

llvm::Value *IRCodegenVisitor::codegen(const ExprConstructorIR &expr) {
  llvm::Type *objType = module->getTypeByName(llvm::StringRef(expr.className));

  // hack - calculate the size of an object in bytes by getting the address of
  // the 1st element of an array that starts at NULL (which has address 0) and
  // casting it to an int
  llvm::Value *objDummyPtr = builder->CreateConstGEP1_64(
      llvm::Constant::getNullValue(objType->getPointerTo()), 1, "objsize");
  llvm::Value *objSize =
      builder->CreatePointerCast(objDummyPtr, llvm::Type::getInt64Ty(*context));

  // allocate the object on the heap and cast void * pointer
  llvm::Value *objVoidPtr =
      builder->CreateCall(module->getFunction("malloc"), objSize);
  llvm::Value *obj =
      builder->CreatePointerCast(objVoidPtr, objType->getPointerTo());

  for (auto &arg : expr.constructorArgs) {
    llvm::Value *argValue = arg->argument->accept(*this);
    llvm::Value *field =
        builder->CreateStructGEP(objType, obj, arg->fieldIndex);
    builder->CreateStore(argValue, field);
  }

  // return the pointer to the object on the heap
  return obj;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprLetIR &expr) {
  llvm::Value *boundVal = expr.boundExpr->accept(*this);

  // put allocainst in entry block of parent function, to be optimised by
  // mem2reg
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  llvm::IRBuilder<> TmpBuilder(&(parentFunction->getEntryBlock()),
                               parentFunction->getEntryBlock().begin());
  llvm::AllocaInst *var = TmpBuilder.CreateAlloca(boundVal->getType(), nullptr,
                                                  llvm::Twine(expr.varName));
  varEnv[expr.varName] = var;
  builder->CreateStore(boundVal, var);
  return boundVal;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprAssignIR &expr) {
  llvm::Value *assignedVal = expr.assignedExpr->accept(*this);
  llvm::Value *id = expr.identifier->accept(*this);
  builder->CreateStore(assignedVal, id);
  return assignedVal;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprConsumeIR &expr) {
  llvm::Value *id = expr.identifier->accept(*this);
  llvm::Value *origVal = builder->CreateLoad(id);
  builder->CreateStore(llvm::Constant::getNullValue(origVal->getType()), id);
  return origVal;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprFunctionAppIR &expr) {
  llvm::Function *calleeFun =
      module->getFunction(llvm::StringRef(expr.functionName));
  std::vector<llvm::Value *> argVals;
  for (auto &arg : expr.arguments) {
    argVals.push_back(arg->accept(*this));
  }
  return builder->CreateCall(calleeFun, argVals);
};

llvm::Value *IRCodegenVisitor::codegen(const ExprIfElseIR &expr) {
  llvm::Value *condValue = expr.condExpr->accept(*this);

  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();

  //  create basic blocks
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(*context, "then", parentFunction);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*context, "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*context, "ifcont");

  builder->CreateCondBr(condValue, thenBB, elseBB);

  // then basic block
  builder->SetInsertPoint(thenBB);
  // then val is that of last value in block
  llvm::Value *thenVal;
  for (auto &thenExpr : expr.thenExpr) {
    thenVal = thenExpr->accept(*this);
  }

  // note that the recursive thenExpr codegen call could change block we're
  // emitting code into.
  thenBB = builder->GetInsertBlock();
  builder->CreateBr(mergeBB);

  // else block
  parentFunction->getBasicBlockList().push_back(elseBB);
  builder->SetInsertPoint(elseBB);

  // else val is that of last value in block
  llvm::Value *elseVal;
  for (auto &elseExpr : expr.elseExpr) {
    elseVal = elseExpr->accept(*this);
  }

  // ditto reasoning to then block
  elseBB = builder->GetInsertBlock();
  builder->CreateBr(mergeBB);

  // merge block
  parentFunction->getBasicBlockList().push_back(mergeBB);
  builder->SetInsertPoint(mergeBB);
  llvm::PHINode *phiNode = builder->CreatePHI(thenVal->getType(), 2, "iftmp");
  phiNode->addIncoming(thenVal, thenBB);
  phiNode->addIncoming(elseVal, elseBB);
  return phiNode;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprWhileLoopIR &expr) {
  llvm::Value *condValue = expr.condExpr->accept(*this);

  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();

  //  create basic blocks
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(*context, "loop");
  llvm::BasicBlock *loopEndBB = llvm::BasicBlock::Create(*context, "loopend");

  // check if we should enter loop
  builder->CreateCondBr(condValue, loopBB, loopEndBB);

  // loop basic block
  parentFunction->getBasicBlockList().push_back(loopBB);
  builder->SetInsertPoint(loopBB);
  for (auto &loopExpr : expr.loopExpr) {
    loopExpr->accept(*this);
  }
  condValue = expr.condExpr->accept(*this);
  if (!condValue) {
    throw IRCodegenException("Foo");
  }
  // note that the recursive loopExpr codegen calls could change block we're
  // emitting code into.
  loopBB = builder->GetInsertBlock();
  builder->CreateCondBr(condValue, loopBB, loopEndBB);

  // loop end expr

  parentFunction->getBasicBlockList().push_back(loopEndBB);
  builder->SetInsertPoint(loopEndBB);

  // loops return void
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*context));
};
llvm::Value *IRCodegenVisitor::codegen(const ExprBinOpIR &expr) {
  llvm::Value *expr1Val = expr.expr1->accept(*this);
  llvm::Value *expr2Val = expr.expr2->accept(*this);
  switch (expr.op) {
    case BinOpPlus:
      return builder->CreateAdd(expr1Val, expr2Val, "add");
    case BinOpMinus:
      return builder->CreateSub(expr1Val, expr2Val, "sub");
    case BinOpMult:
      return builder->CreateMul(expr1Val, expr2Val, "mult");
    case BinOpIntDiv:
      return builder->CreateSDiv(expr1Val, expr2Val, "div");
    case BinOpRem:
      return builder->CreateSRem(expr1Val, expr2Val, "rem");
    case BinOpLessThan:
      return builder->CreateICmpSLT(expr1Val, expr2Val, "lt");
    case BinOpLessThanEq:
      return builder->CreateICmpSLE(expr1Val, expr2Val, "lte");
    case BinOpGreaterThan:
      return builder->CreateICmpSGT(expr1Val, expr2Val, "gt");
    case BinOpGreaterThanEq:
      return builder->CreateICmpSGE(expr1Val, expr2Val, "gte");
    case BinOpAnd:
      return builder->CreateAnd(expr1Val, expr2Val, "and");
    case BinOpOr:
      return builder->CreateOr(expr1Val, expr2Val, "or");
    case BinOpEq:
      return builder->CreateICmpEQ(expr1Val, expr2Val, "eq");
    case BinOpNotEq:
      return builder->CreateICmpNE(expr1Val, expr2Val, "neq");
  }
};
llvm::Value *IRCodegenVisitor::codegen(const ExprUnOpIR &expr) {
  llvm::Value *exprVal = expr.expr->accept(*this);
  switch (expr.op) {
    case UnOpNot:
      return builder->CreateNot(exprVal, "not");
    case UnOpNeg:
      return builder->CreateNeg(exprVal, "neg");
  }
};

llvm::Value *IRCodegenVisitor::codegen(
    const ExprFinishAsyncIR &finishAsyncExpr) {
  // TODO: Add support for concurrency
  std::vector<llvm::Value *> pthreadPtrPtrs;

  for (auto &asyncExpr : finishAsyncExpr.asyncExprs) {
    llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();
    llvm::Value *pthreadPtrPtr =
        builder->CreateAlloca(voidPtrTy, nullptr, llvm::Twine("pthread"));
    pthreadPtrPtrs.push_back(pthreadPtrPtr);
    codegenCreatePThread(pthreadPtrPtr, *asyncExpr);
  };
  llvm::Value *exprVal;
  for (auto &expr : finishAsyncExpr.currentThreadExpr) {
    exprVal = expr->accept(*this);
  }
  codegenJoinPThreads(pthreadPtrPtrs);
  return exprVal;
}
void IRCodegenVisitor::codegenJoinPThreads(
    const std::vector<llvm::Value *> pthreadPtrPtrs) {
  llvm::Function *pthread_join =
      module->getFunction(llvm::StringRef("pthread_join"));
  llvm::Type *voidPtrPtrTy =
      llvm::Type::getInt8Ty(*context)->getPointerTo()->getPointerTo();
  for (auto &pthreadPtrPtr : pthreadPtrPtrs) {
    llvm::Value *pthreadPtr = builder->CreateLoad(pthreadPtrPtr);
    builder->CreateCall(
        pthread_join, {pthreadPtr, llvm::Constant::getNullValue(voidPtrPtrTy)});
  }
}

void IRCodegenVisitor::codegenCreatePThread(llvm::Value *pthreadPtr,
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
      pthreadPtr,
      voidPtrNull,
      asyncFun,
      builder->CreatePointerCast(asyncFunArg, voidPtrTy),
  };
  builder->CreateCall(pthread_create, args);
}

llvm::Function *IRCodegenVisitor::codegenAsyncFunction(
    const AsyncExprIR &asyncExpr, llvm::StructType *functionArgType,
    llvm::Type *functionArgPointerType) {
  // find unique function name
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
    expr->accept(*this);
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

llvm::Value *IRCodegenVisitor::codegen(const ExprPrintfIR &expr) {
  llvm::Function *printf = module->getFunction("printf");
  std::vector<llvm::Value *> printfArgs;
  printfArgs.push_back(builder->CreateGlobalStringPtr(expr.formatStr + '\n'));
  for (auto &arg : expr.arguments) {
    printfArgs.push_back(arg->accept(*this));
  }
  return builder->CreateCall(printf, printfArgs);
};

llvm::Value *IRCodegenVisitor::codegen(const ExprBlockIR &blockExpr) {
  llvm::Value *lastExprVal;
  for (auto &expr : blockExpr.exprs) {
    lastExprVal = (expr->accept(*this));
  }
  return lastExprVal;
}