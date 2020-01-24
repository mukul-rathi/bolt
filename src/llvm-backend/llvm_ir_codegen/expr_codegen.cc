
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ThreadPool.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

llvm::Value *IRCodegenVisitor::codegen(const IdentifierVarIR &var) {
  return varEnv[var.varName];
};
llvm::Value *IRCodegenVisitor::codegen(const IdentifierObjFieldIR &objField) {
  llvm::AllocaInst *var = varEnv[objField.objName];

  return builder->CreateStructGEP(var->getAllocatedType(), var,
                                  objField.fieldIndex);
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
  llvm::AllocaInst *obj = builder->CreateAlloca(
      module->getTypeByName(llvm::StringRef(expr.className)), nullptr,
      llvm::Twine(expr.className));
  for (auto &arg : expr.constructorArgs) {
    llvm::Value *argValue = arg->argument->accept(*this);

    llvm::Value *field =
        builder->CreateStructGEP(obj->getAllocatedType(), obj, arg->fieldIndex);
    builder->CreateStore(argValue, field);
  }
  return builder->CreateLoad(obj);
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

llvm::Value *IRCodegenVisitor::codegen(const ExprFinishAsyncIR &expr) {
  // TODO: Add support for concurrency
  return nullptr;
};