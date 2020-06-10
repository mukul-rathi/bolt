
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
  llvm::Value *val = varEnv[var.varName];
  if (val == nullptr) {
    throw new IRCodegenException(std::string("Var not found: " + var.varName));
  }
  return val;
};

llvm::Value *IRCodegenVisitor::codegen(const IdentifierObjFieldIR &objField) {
  llvm::AllocaInst *objPtr =
      varEnv[objField.varName];  // pointer to value of variable stack
  if (objPtr == nullptr) {
    throw new IRCodegenException(
        std::string("Objectœ not found: " + objField.varName));
  }
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
  if (id == nullptr) {
    throw new IRCodegenException(
        std::string("Identifier not found: " + expr.identifier->varName));
  }
  if (expr.shouldLock) {
    (ExprLockIR(expr.identifier->varName, expr.lockType)).accept(*this);
  }
  llvm::Value *idVal = builder->CreateLoad(id);
  if (expr.shouldLock) {
    (ExprUnlockIR(expr.identifier->varName, expr.lockType)).accept(*this);
  }
  if (idVal == nullptr) {
    throw new IRCodegenException(
        std::string("Identifier not loaded: " + expr.identifier->varName));
  }
  return idVal;
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
  std::string vTableName = "_Vtable" + expr.className;
  llvm::Value *vTableField = builder->CreateStructGEP(objType, obj, 0);
  llvm::Value *vTable = module->getNamedGlobal(vTableName);
  if (vTable == nullptr) {
    throw new IRCodegenException(
        std::string("Can't get vTable: " + vTableName));
  }
  builder->CreateStore(vTable, vTableField);
  // set lock counters to zero
  llvm::Value *zeroVal =
      llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 0);
  llvm::Value *readLockCounter =
      builder->CreateStructGEP(objType, obj, LockType::Reader);
  builder->CreateStore(zeroVal, readLockCounter);
  llvm::Value *writeLockCounter =
      builder->CreateStructGEP(objType, obj, LockType::Writer);
  builder->CreateStore(zeroVal, writeLockCounter);

  for (auto &arg : expr.constructorArgs) {
    if (arg == nullptr || arg->argument == nullptr) {
      throw new IRCodegenException(
          std::string("Null constructor arg for " + expr.className));
    }
    llvm::Value *argValue = arg->argument->accept(*this);
    llvm::Value *field =
        builder->CreateStructGEP(objType, obj, arg->fieldIndex);
    builder->CreateStore(argValue, field);
  }

  // return the pointer to the object on the heap
  return obj;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprLetIR &expr) {
  if (expr.boundExpr == nullptr) {
    throw new IRCodegenException(
        std::string("Let - binding a null expr to " + expr.varName));
  }
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
  if (expr.assignedExpr == nullptr) {
    throw new IRCodegenException(
        std::string("Assigning a null expr to " + expr.identifier->varName));
  }
  llvm::Value *assignedVal = expr.assignedExpr->accept(*this);
  llvm::Value *id = expr.identifier->accept(*this);
  if (id == nullptr) {
    throw new IRCodegenException(std::string("Trying to assign to a null id: " +
                                             expr.identifier->varName));
  }
  if (expr.shouldLock) {
    (ExprLockIR(expr.identifier->varName, expr.lockType)).accept(*this);
  }
  builder->CreateStore(assignedVal, id);
  if (expr.shouldLock) {
    (ExprUnlockIR(expr.identifier->varName, expr.lockType)).accept(*this);
  }
  return assignedVal;
};
llvm::Value *IRCodegenVisitor::codegen(const ExprConsumeIR &expr) {
  llvm::Value *id = expr.identifier->accept(*this);
  if (id == nullptr) {
    throw new IRCodegenException(std::string("Trying to consume a null id: " +
                                             expr.identifier->varName));
  }
  if (expr.shouldLock) {
    (ExprLockIR(expr.identifier->varName, expr.lockType)).accept(*this);
  }
  llvm::Value *origVal = builder->CreateLoad(id);
  builder->CreateStore(llvm::Constant::getNullValue(origVal->getType()), id);
  if (expr.shouldLock) {
    (ExprUnlockIR(expr.identifier->varName, expr.lockType)).accept(*this);
  }
  return origVal;
};

llvm::Value *IRCodegenVisitor::codegen(const ExprFunctionAppIR &expr) {
  llvm::Function *calleeFun =
      module->getFunction(llvm::StringRef(expr.functionName));
  if (calleeFun == nullptr) {
    throw new IRCodegenException(
        std::string("Function doesn't exist: " + expr.functionName));
  }
  std::vector<llvm::Value *> argVals;
  for (auto &arg : expr.arguments) {
    llvm ::Value *argVal = arg->accept(*this);
    if (argVal == nullptr) {
      throw new IRCodegenException(std::string(
          "Null Argument when calling function " + expr.functionName));
    }
    argVals.push_back(argVal);
  }
  return builder->CreateCall(calleeFun, argVals);
};

llvm::Value *IRCodegenVisitor::codegen(const ExprMethodAppIR &expr) {
  llvm::Value *thisObj = builder->CreateLoad(varEnv[expr.objName]);
  if (thisObj == nullptr) {
    throw new IRCodegenException(std::string("Method called on null object"));
  }
  llvm::Value *vTablePtr = builder->CreateLoad(builder->CreateStructGEP(
      thisObj->getType()
          ->getPointerElementType() /* get type of element on heap*/,
      thisObj, 0));
  llvm::Value *calleeMethodPtr =
      builder->CreateStructGEP(vTablePtr->getType()->getPointerElementType(),
                               vTablePtr, expr.methodIndex);

  llvm::Value *calleeMethod = (builder->CreateLoad(calleeMethodPtr));
  if (calleeMethod == nullptr) {
    throw new IRCodegenException(
        std::string("Method doesn't exist: " + expr.objName + "->" +
                    std::to_string(expr.methodIndex)));
  }
  std::vector<llvm::Value *> argVals{thisObj};
  for (auto &arg : expr.arguments) {
    llvm ::Value *argVal = arg->accept(*this);
    if (argVal == nullptr) {
      throw new IRCodegenException(
          std::string("Null Argument when calling method " + expr.objName +
                      "->" + std::to_string(expr.methodIndex)));
    }
    argVals.push_back(argVal);
  }
  return builder->CreateCall(calleeMethod, argVals);
};

llvm::Value *IRCodegenVisitor::codegen(const ExprIfElseIR &expr) {
  llvm::Value *condValue = expr.condExpr->accept(*this);
  if (condValue == nullptr) {
    throw new IRCodegenException(
        std::string("Null condition expr for if-else statement"));
  }
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
  if (thenVal == nullptr) {
    throw new IRCodegenException(
        std::string("Null then expr for if-else statement"));
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
  if (elseVal == nullptr) {
    throw new IRCodegenException(
        std::string("Null else expr for if-else statement"));
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
  if (condValue == nullptr) {
    throw new IRCodegenException(
        std::string("Null condition expr for while statement"));
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
  if (expr1Val == nullptr || expr2Val == nullptr) {
    throw new IRCodegenException(std::string("Bin-op operand is null"));
  }
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
  if (exprVal == nullptr) {
    throw new IRCodegenException(std::string("Unary op's operand is null"));
  }
  switch (expr.op) {
    case UnOpNot:
      return builder->CreateNot(exprVal, "not");
    case UnOpNeg:
      return builder->CreateNeg(exprVal, "neg");
  }
};

llvm::Value *IRCodegenVisitor::codegen(
    const ExprFinishAsyncIR &finishAsyncExpr) {
  std::vector<llvm::Value *> pthreadPtrPtrs;

  for (auto &asyncExpr : finishAsyncExpr.asyncExprs) {
    if (asyncExpr == nullptr) {
      throw new IRCodegenException(
          std::string("Null async expr in finish-async statement"));
    }
    llvm::Type *pthreadPtrTy =
        module->getTypeByName(llvm::StringRef("pthread_t"))->getPointerTo();
    llvm::Value *pthreadPtrPtr =
        builder->CreateAlloca(pthreadPtrTy, nullptr, llvm::Twine("pthreadPtr"));
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

llvm::Value *IRCodegenVisitor::codegen(const ExprPrintfIR &expr) {
  llvm::Function *printf = module->getFunction("printf");
  std::vector<llvm::Value *> printfArgs;
  printfArgs.push_back(builder->CreateGlobalStringPtr(expr.formatStr));
  for (auto &arg : expr.arguments) {
    llvm::Value *argVal = arg->accept(*this);
    if (argVal == nullptr) {
      throw new IRCodegenException(std::string("Printf has null arg"));
    }
    printfArgs.push_back(argVal);
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
