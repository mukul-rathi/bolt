#pragma once
#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
/* Operator IR */

enum BinOp {
  BinOpPlus,
  BinOpMinus,
  BinOpMult,
  BinOpIntDiv,
  BinOpRem,
  BinOpLessThan,
  BinOpLessThanEq,
  BinOpGreaterThan,
  BinOpGreaterThanEq,
  BinOpAnd,
  BinOpOr,
  BinOpEq,
  BinOpNotEq
};

enum UnOp { UnOpNot, UnOpNeg };

/* Identifier IR */

struct IdentifierIR {
  virtual ~IdentifierIR() = default;
};
std::unique_ptr<IdentifierIR> deserialiseIdentifier(
    const Frontend_ir::identifier &identifier);

struct IdentifierVarIR : public IdentifierIR {
  std::string varName;
  IdentifierVarIR(const std::string &name) : varName(name) {}
};

struct IdentifierObjFieldIR : public IdentifierIR {
  std::string objName;
  int fieldIndex;
  IdentifierObjFieldIR(const Frontend_ir::identifier::_ObjField &objfield);
};

/* Expression IR */

struct ExprIR {
  virtual ~ExprIR() = default;
};

std::unique_ptr<ExprIR> deserialiseExpr(const Frontend_ir::expr &expr);

struct ConstructorArgIR {
  int fieldIndex;
  std::unique_ptr<ExprIR> argument;
  ConstructorArgIR(const Frontend_ir::constructor_arg &constr_arg);
};

struct ExprUnitIR : public ExprIR {};

struct ExprIntegerIR : public ExprIR {
  int val;
  ExprIntegerIR(const int &i) : val(i) {}
};
struct ExprBooleanIR : public ExprIR {
  bool val;
  ExprBooleanIR(const bool &b) : val(b) {}
};

struct ExprIdentifierIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  ExprIdentifierIR(const Frontend_ir::identifier &id)
      : identifier(deserialiseIdentifier(id)) {}
};

struct ExprConstructorIR : public ExprIR {
  std::string className;
  std::vector<std::unique_ptr<ConstructorArgIR>> constructorArgs;
  ExprConstructorIR(const Frontend_ir::expr::_Constructor &constr);
};

struct ExprLetIR : public ExprIR {
  std::string varName;
  std::unique_ptr<ExprIR> boundExpr;
  ExprLetIR(const Frontend_ir::expr::_Let &letExpr);
};

struct ExprAssignIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  std::unique_ptr<ExprIR> assignedExpr;
  ExprAssignIR(const Frontend_ir::expr::_Assign &expr);
};

struct ExprConsumeIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  ExprConsumeIR(const Frontend_ir::identifier &id)
      : identifier(deserialiseIdentifier(id)){};
};

struct ExprFunctionAppIR : public ExprIR {
  std::string functionName;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprFunctionAppIR(const Frontend_ir::expr::_FunctionApp &expr);
};

struct ExprFinishAsyncIR : public ExprIR {
  std::vector<std::unique_ptr<std::vector<std::unique_ptr<ExprIR>>>> asyncExprs;
  std::vector<std::unique_ptr<ExprIR>> currentThreadExpr;
  ExprFinishAsyncIR(const Frontend_ir::expr::_FinishAsync &expr);
};

struct ExprIfElseIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> thenExpr;
  std::vector<std::unique_ptr<ExprIR>> elseExpr;
  ExprIfElseIR(const Frontend_ir::expr::_IfElse &expr);
};

struct ExprWhileLoopIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> loopExpr;
  ExprWhileLoopIR(const Frontend_ir::expr::_WhileLoop &expr);
};

struct ExprBinOpIR : public ExprIR {
  enum BinOp op;
  std::unique_ptr<ExprIR> expr1;
  std::unique_ptr<ExprIR> expr2;
  ExprBinOpIR(const Frontend_ir::expr::_BinOp &expr);
};

struct ExprUnOpIR : public ExprIR {
  enum UnOp op;
  std::unique_ptr<ExprIR> expr;
  ExprUnOpIR(const Frontend_ir::expr::_UnOp &expr);
};