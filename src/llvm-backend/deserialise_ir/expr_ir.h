#pragma once
#include <string>
#include <stdlib.h>
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

class IdentifierIR {
 public:
  virtual ~IdentifierIR() = default;
};

class IdentifierVarIR : public IdentifierIR {
  std::string varName;
};

class IdentifierObjFieldIR : public IdentifierIR {
  std::string objName;
  int fieldIndex;
};

/* Expression IR */

class ExprIR {
 public:
  virtual ~ExprIR();
};

class ConstructorArgIR {
  int fieldIndex;
  std::unique_ptr<ExprIR> argument;
};

class ExprUnitIR : public ExprIR {};

class ExprIntIR : public ExprIR {
  int val;
};

class ExprIdentifierIR : public ExprIR {
  IdentifierIR identifier;
};

class ExprConstructorIR : public ExprIR {
  std::string className;
  std::vector<std::unique_ptr<ConstructorArgIR>> constructorArgs;
};

class ExprLetIR : public ExprIR {
  std::string varName;
  std::unique_ptr<ExprIR> boundExpr;
};

class ExprAssignIR : public ExprIR {
  IdentifierIR identifier;
  std::unique_ptr<ExprIR> assignedExpr;
};

class ExprConsumeIR : public ExprIR {
  IdentifierIR identifier;
  ;
};

class ExprFunctionAppIR : public ExprIR {
  std::string functionName;
  std::vector<std::unique_ptr<ExprIR>> arguments;
};

class ExprFinishAsyncIR : public ExprIR {
  std::vector<std::vector<std::unique_ptr<ExprIR>>> asyncThreadsExprs;
  std::vector<std::unique_ptr<ExprIR>> currentThreadExprs;
};

class ExprIfIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> thenExprs;
  std::vector<std::unique_ptr<ExprIR>> elseExprs;
};

class ExprWhileIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> loopExpr;
};

class ExprBinOpIR : public ExprIR {
  BinOp op;
  std::unique_ptr<ExprIR> expr1;
  std::unique_ptr<ExprIR> expr2;
};

class ExprUnOpIR : public ExprIR {
  UnOp op;
  std::unique_ptr<ExprIR> expr;
};