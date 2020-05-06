#include "src/llvm-backend/deserialise_ir/expr_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "llvm/IR/Value.h"
#include "src/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/ir_visitor.h"

/* Operator IR */

enum UnOp deserialiseUnOp(const Frontend_ir::un_op &op) {
  switch (op.tag()) {
    case Frontend_ir::un_op__tag_UnOpNot_tag:
      return UnOp::UnOpNot;
    case Frontend_ir::un_op__tag_UnOpNeg_tag:
      return UnOp::UnOpNeg;
  }
}

enum BinOp deserialiseBinOp(const Frontend_ir::bin_op &op) {
  switch (op.tag()) {
    case Frontend_ir::bin_op__tag_BinOpPlus_tag:
      return BinOp::BinOpPlus;
    case Frontend_ir::bin_op__tag_BinOpMinus_tag:
      return BinOp::BinOpMinus;
    case Frontend_ir::bin_op__tag_BinOpMult_tag:
      return BinOp::BinOpMult;
    case Frontend_ir::bin_op__tag_BinOpIntDiv_tag:
      return BinOp::BinOpIntDiv;
    case Frontend_ir::bin_op__tag_BinOpRem_tag:
      return BinOp::BinOpRem;
    case Frontend_ir::bin_op__tag_BinOpLessThan_tag:
      return BinOp::BinOpLessThan;
    case Frontend_ir::bin_op__tag_BinOpLessThanEq_tag:
      return BinOp::BinOpLessThanEq;
    case Frontend_ir::bin_op__tag_BinOpGreaterThan_tag:
      return BinOp::BinOpGreaterThan;
    case Frontend_ir::bin_op__tag_BinOpGreaterThanEq_tag:
      return BinOp::BinOpGreaterThanEq;
    case Frontend_ir::bin_op__tag_BinOpAnd_tag:
      return BinOp::BinOpAnd;
    case Frontend_ir::bin_op__tag_BinOpOr_tag:
      return BinOp::BinOpOr;
    case Frontend_ir::bin_op__tag_BinOpEq_tag:
      return BinOp::BinOpEq;
    case Frontend_ir::bin_op__tag_BinOpNotEq_tag:
      return BinOp::BinOpNotEq;
  }
}

/* Identifier IR */
std::unique_ptr<IdentifierIR> deserialiseIdentifier(
    const Frontend_ir::identifier &identifier) {
  switch (identifier.tag()) {
    case Frontend_ir::identifier__tag_Variable_tag:
      return std::unique_ptr<IdentifierIR>(
          new IdentifierVarIR(identifier.variable()));
    case Frontend_ir::identifier__tag_ObjField_tag:
      return std::unique_ptr<IdentifierIR>(
          new IdentifierObjFieldIR(identifier.objfield()));
  }
};

IdentifierVarIR::IdentifierVarIR(const std::string &name) { varName = name; }

llvm::Value *IdentifierVarIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

IdentifierObjFieldIR::IdentifierObjFieldIR(
    const Frontend_ir::identifier::_ObjField &objfield) {
  varName = objfield._0();
  fieldIndex = objfield._1();
}
llvm::Value *IdentifierObjFieldIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

/* Lock IR */

LockType deserialiseLockType(const Frontend_ir::lock_type &lockType) {
  switch (lockType.tag()) {
    case Frontend_ir::lock_type__tag_Reader_tag:
      return LockType::Reader;
    case Frontend_ir::lock_type__tag_Writer_tag:
      return LockType::Writer;
  }
}

/* Expression IR */

std::unique_ptr<ExprIR> deserialiseExpr(const Frontend_ir::expr &expr) {
  switch (expr.tag()) {
    case Frontend_ir::expr__tag_Integer_tag:
      return std::unique_ptr<ExprIR>(new ExprIntegerIR(expr.integer()));
    case Frontend_ir::expr__tag_Boolean_tag:
      return std::unique_ptr<ExprIR>(new ExprBooleanIR(expr.boolean()));
    case Frontend_ir::expr__tag_Identifier_tag:
      return std::unique_ptr<ExprIR>(new ExprIdentifierIR(expr.identifier()));
    case Frontend_ir::expr__tag_Constructor_tag:
      return std::unique_ptr<ExprIR>(new ExprConstructorIR(expr.constructor()));
    case Frontend_ir::expr__tag_Let_tag:
      return std::unique_ptr<ExprIR>(new ExprLetIR(expr.let()));
    case Frontend_ir::expr__tag_Assign_tag:
      return std::unique_ptr<ExprIR>(new ExprAssignIR(expr.assign()));
    case Frontend_ir::expr__tag_Consume_tag:
      return std::unique_ptr<ExprIR>(new ExprConsumeIR(expr.consume()));
    case Frontend_ir::expr__tag_FunctionApp_tag:
      return std::unique_ptr<ExprIR>(new ExprFunctionAppIR(expr.functionapp()));
    case Frontend_ir::expr__tag_MethodApp_tag:
      return std::unique_ptr<ExprIR>(new ExprMethodAppIR(expr.methodapp()));
    case Frontend_ir::expr__tag_FinishAsync_tag:
      return std::unique_ptr<ExprIR>(new ExprFinishAsyncIR(expr.finishasync()));
    case Frontend_ir::expr__tag_IfElse_tag:
      return std::unique_ptr<ExprIR>(new ExprIfElseIR(expr.ifelse()));
    case Frontend_ir::expr__tag_WhileLoop_tag:
      return std::unique_ptr<ExprIR>(new ExprWhileLoopIR(expr.whileloop()));
    case Frontend_ir::expr__tag_BinOp_tag:
      return std::unique_ptr<ExprIR>(new ExprBinOpIR(expr.binop()));
    case Frontend_ir::expr__tag_UnOp_tag:
      return std::unique_ptr<ExprIR>(new ExprUnOpIR(expr.unop()));
    case Frontend_ir::expr__tag_Printf_tag:
      return std::unique_ptr<ExprIR>(new ExprPrintfIR(expr.printf()));
    case Frontend_ir::expr__tag_Block_tag:
      return std::unique_ptr<ExprIR>(new ExprBlockIR(expr.block()));
    case Frontend_ir::expr__tag_Lock_tag:
      return std::unique_ptr<ExprIR>(new ExprLockIR(expr.lock()));
    case Frontend_ir::expr__tag_Unlock_tag:
      return std::unique_ptr<ExprIR>(new ExprUnlockIR(expr.unlock()));
  }
}

llvm::Value *ExprIntegerIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *ExprBooleanIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprIdentifierIR::ExprIdentifierIR(const Frontend_ir::expr::_Identifier &expr) {
  identifier = deserialiseIdentifier(expr._0());
  shouldLock = expr.has__1();
  if (expr.has__1()) {
    lockType = deserialiseLockType(expr._1());
  }
}

llvm::Value *ExprIdentifierIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ConstructorArgIR::ConstructorArgIR(
    const Frontend_ir::constructor_arg &constr_arg) {
  fieldIndex = constr_arg.constructorarg()._0();
  argument = deserialiseExpr(constr_arg.constructorarg()._1());
}

AsyncExprIR::AsyncExprIR(const Frontend_ir::async_expr &asyncExpr) {
  for (int i = 0; i < asyncExpr.asyncexpr()._0().size(); i++) {
    freeVars.push_back(asyncExpr.asyncexpr()._0(i));
  }
  for (int i = 0; i < asyncExpr.asyncexpr()._1().__size(); i++) {
    exprs.push_back(deserialiseExpr(asyncExpr.asyncexpr()._1()._(i)));
  }
}

ExprConstructorIR::ExprConstructorIR(
    const Frontend_ir::expr::_Constructor &constr) {
  className = constr._0();
  for (int i = 0; i < constr._1().size(); i++) {
    constructorArgs.push_back(
        std::unique_ptr<ConstructorArgIR>(new ConstructorArgIR(constr._1(i))));
  }
}

llvm::Value *ExprConstructorIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprLetIR::ExprLetIR(const Frontend_ir::expr::_Let &letExpr) {
  varName = letExpr._0();
  boundExpr = deserialiseExpr(letExpr._1());
}
llvm::Value *ExprLetIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprAssignIR::ExprAssignIR(const Frontend_ir::expr::_Assign &expr) {
  identifier = deserialiseIdentifier(expr._0());
  assignedExpr = deserialiseExpr(expr._1());
  shouldLock = expr.has__2();
  if (expr.has__2()) {
    lockType = deserialiseLockType(expr._2());
  }
}

llvm::Value *ExprAssignIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprConsumeIR::ExprConsumeIR(const Frontend_ir::expr::_Consume &expr) {
  identifier = deserialiseIdentifier(expr._0());
  shouldLock = expr.has__1();
  if (expr.has__1()) {
    lockType = deserialiseLockType(expr._1());
  }
}

llvm::Value *ExprConsumeIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprFunctionAppIR::ExprFunctionAppIR(
    const Frontend_ir::expr::_FunctionApp &expr) {
  functionName = expr._0();
  for (int i = 0; i < expr._1().__size(); i++) {
    arguments.push_back(deserialiseExpr(expr._1()._(i)));
  }
}

llvm::Value *ExprFunctionAppIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprMethodAppIR::ExprMethodAppIR(const Frontend_ir::expr::_MethodApp &expr) {
  objName = expr._0();
  methodName = expr._1();
  for (int i = 0; i < expr._2().size(); i++) {
    arguments.push_back(deserialiseExpr(expr._2(i)));
  }
}

llvm::Value *ExprMethodAppIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprFinishAsyncIR::ExprFinishAsyncIR(
    const Frontend_ir::expr::_FinishAsync &expr) {
  for (int i = 0; i < expr._0_size(); i++) {
    asyncExprs.push_back(
        std::unique_ptr<AsyncExprIR>(new AsyncExprIR(expr._0(i))));
  }

  for (int i = 0; i < expr._1().__size(); i++) {
    currentThreadExpr.push_back(deserialiseExpr(expr._1()._(i)));
  }
}

llvm::Value *ExprFinishAsyncIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprIfElseIR::ExprIfElseIR(const Frontend_ir::expr::_IfElse &expr) {
  condExpr = deserialiseExpr(expr._0());
  for (int i = 0; i < expr._1().__size(); i++) {
    thenExpr.push_back(deserialiseExpr(expr._1()._(i)));
  }
  for (int i = 0; i < expr._2().__size(); i++) {
    elseExpr.push_back(deserialiseExpr(expr._2()._(i)));
  }
}

llvm::Value *ExprIfElseIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprWhileLoopIR::ExprWhileLoopIR(const Frontend_ir::expr::_WhileLoop &expr) {
  condExpr = deserialiseExpr(expr._0());
  for (int i = 0; i < expr._1().__size(); i++) {
    loopExpr.push_back(deserialiseExpr(expr._1()._(i)));
  }
}

llvm::Value *ExprWhileLoopIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprBinOpIR::ExprBinOpIR(const Frontend_ir::expr::_BinOp &expr) {
  op = deserialiseBinOp(expr._0());
  expr1 = deserialiseExpr(expr._1());
  expr2 = deserialiseExpr(expr._2());
}

llvm::Value *ExprBinOpIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprUnOpIR::ExprUnOpIR(const Frontend_ir::expr::_UnOp &unopExpr) {
  op = deserialiseUnOp(unopExpr._0());
  expr = deserialiseExpr(unopExpr._1());
}

llvm::Value *ExprUnOpIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprPrintfIR::ExprPrintfIR(const Frontend_ir::expr::_Printf &expr) {
  formatStr = expr._0();
  for (int i = 0; i < expr._1().__size(); i++) {
    arguments.push_back(deserialiseExpr(expr._1()._(i)));
  }
}

llvm::Value *ExprPrintfIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprBlockIR::ExprBlockIR(const Frontend_ir::exprs &expr) {
  for (int i = 0; i < expr.__size(); i++) {
    exprs.push_back(deserialiseExpr(expr._(i)));
  }
}

llvm::Value *ExprBlockIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprLockIR::ExprLockIR(const Frontend_ir::expr::_Lock &expr) {
  objName = expr._0();
  lockType = deserialiseLockType(expr._1());
}

llvm::Value *ExprLockIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

ExprUnlockIR::ExprUnlockIR(const Frontend_ir::expr::_Unlock &expr) {
  objName = expr._0();
  lockType = deserialiseLockType(expr._1());
}

llvm::Value *ExprUnlockIR::accept(IRVisitor &visitor) {
  return visitor.codegen(*this);
}