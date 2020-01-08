#pragma once
#include <stdlib.h>

#include <string>
#include <vector>
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
 public:
  virtual ~IdentifierIR() = default;
};

struct IdentifierVarIR : public IdentifierIR {
  std::string varName;
};

struct IdentifierObjFieldIR : public IdentifierIR {
  std::string objName;
  int fieldIndex;
};

/* Expression IR */

struct ExprIR {
 public:
  virtual ~ExprIR();
};

struct ConstructorArgIR {
  int fieldIndex;
  std::unique_ptr<ExprIR> argument;
};

struct ExprUnitIR : public ExprIR {};

struct ExprIntIR : public ExprIR {
  int val;
};

struct ExprIdentifierIR : public ExprIR {
  IdentifierIR identifier;
};

struct ExprConstructorIR : public ExprIR {
  std::string className;
  std::vector<std::unique_ptr<ConstructorArgIR>> constructorArgs;
};

struct ExprLetIR : public ExprIR {
  std::string varName;
  std::unique_ptr<ExprIR> boundExpr;
};

struct ExprAssignIR : public ExprIR {
  IdentifierIR identifier;
  std::unique_ptr<ExprIR> assignedExpr;
};

struct ExprConsumeIR : public ExprIR {
  IdentifierIR identifier;
  ;
};

struct ExprFunctionAppIR : public ExprIR {
  std::string functionName;
  std::vector<std::unique_ptr<ExprIR>> arguments;
};

struct ExprFinishAsyncIR : public ExprIR {
  std::vector<std::vector<std::unique_ptr<ExprIR>>> asyncThreadsExprs;
  std::vector<std::unique_ptr<ExprIR>> currentThreadExprs;
};

struct ExprIfIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> thenExprs;
  std::vector<std::unique_ptr<ExprIR>> elseExprs;
};

struct ExprWhileIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> loopExpr;
};

struct ExprBinOpIR : public ExprIR {
  BinOp op;
  std::unique_ptr<ExprIR> expr1;
  std::unique_ptr<ExprIR> expr2;
};

struct ExprUnOpIR : public ExprIR {
  UnOp op;
  std::unique_ptr<ExprIR> expr;
};