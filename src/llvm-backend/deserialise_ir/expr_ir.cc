#include "src/llvm-backend/deserialise_ir/expr_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
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
void IdentifierVarIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

IdentifierObjFieldIR::IdentifierObjFieldIR(
    const Frontend_ir::identifier::_ObjField &objfield) {
  objName = objfield._0();
  fieldIndex = objfield._1();
}
void IdentifierObjFieldIR::accept(IRVisitor &visitor) {
  visitor.codegen(*this);
}

/* Expression IR */

std::unique_ptr<ExprIR> deserialiseExpr(const Frontend_ir::expr &expr) {
  switch (expr.tag()) {
    case Frontend_ir::expr__tag_Unit_tag:
      return std::unique_ptr<ExprIR>(new ExprUnitIR());
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
  }
}

void ExprUnitIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
void ExprIntegerIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
void ExprBooleanIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
void ExprIdentifierIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ConstructorArgIR::ConstructorArgIR(
    const Frontend_ir::constructor_arg &constr_arg) {
  fieldIndex = constr_arg.constructorarg()._0();
  argument = deserialiseExpr(constr_arg.constructorarg()._1());
}

ExprConstructorIR::ExprConstructorIR(
    const Frontend_ir::expr::_Constructor &constr) {
  className = constr._0();
  for (int i = 0; i < constr._1().size(); i++) {
    constructorArgs.push_back(
        std::unique_ptr<ConstructorArgIR>(new ConstructorArgIR(constr._1(i))));
  }
}

void ExprConstructorIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprLetIR::ExprLetIR(const Frontend_ir::expr::_Let &letExpr) {
  varName = letExpr._0();
  boundExpr = deserialiseExpr(letExpr._1());
}
void ExprLetIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprAssignIR::ExprAssignIR(const Frontend_ir::expr::_Assign &expr) {
  identifier = deserialiseIdentifier(expr._0());
  assignedExpr = deserialiseExpr(expr._1());
}

void ExprAssignIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
void ExprConsumeIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprFunctionAppIR::ExprFunctionAppIR(
    const Frontend_ir::expr::_FunctionApp &expr) {}

void ExprFunctionAppIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprFinishAsyncIR::ExprFinishAsyncIR(
    const Frontend_ir::expr::_FinishAsync &expr) {
  for (int i = 0; i < expr._0_size(); i++) {
    Frontend_ir::exprs asyncExpr = expr._0(i);
    std::unique_ptr<std::vector<std::unique_ptr<ExprIR>>> asyncExprVec(
        new std::vector<std::unique_ptr<ExprIR>>());

    for (int j = 0; j < asyncExpr.__size(); j++) {
      asyncExprVec->push_back(deserialiseExpr(asyncExpr._(j)));
    }

    asyncExprs.push_back(std::move(asyncExprVec));
  }

  for (int i = 0; i < expr._1().__size(); i++) {
    currentThreadExpr.push_back(deserialiseExpr(expr._1()._(i)));
  }
}

void ExprFinishAsyncIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprIfElseIR::ExprIfElseIR(const Frontend_ir::expr::_IfElse &expr) {
  condExpr = deserialiseExpr(expr._0());
  for (int i = 0; i < expr._1().__size(); i++) {
    thenExpr.push_back(deserialiseExpr(expr._1()._(i)));
  }
  for (int i = 0; i < expr._2().__size(); i++) {
    elseExpr.push_back(deserialiseExpr(expr._2()._(i)));
  }
}

void ExprIfElseIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprWhileLoopIR::ExprWhileLoopIR(const Frontend_ir::expr::_WhileLoop &expr) {
  condExpr = deserialiseExpr(expr._0());
  for (int i = 0; i < expr._1().__size(); i++) {
    loopExpr.push_back(deserialiseExpr(expr._1()._(i)));
  }
}

void ExprWhileLoopIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprBinOpIR::ExprBinOpIR(const Frontend_ir::expr::_BinOp &expr) {
  op = deserialiseBinOp(expr._0());
  expr1 = deserialiseExpr(expr._1());
  expr2 = deserialiseExpr(expr._2());
}

void ExprBinOpIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }

ExprUnOpIR::ExprUnOpIR(const Frontend_ir::expr::_UnOp &unopExpr) {
  op = deserialiseUnOp(unopExpr._0());
  expr = deserialiseExpr(unopExpr._1());
}

void ExprUnOpIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
