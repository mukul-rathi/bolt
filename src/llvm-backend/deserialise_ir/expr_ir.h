#pragma once
#include <stdlib.h>

#include <string>
#include <vector>

#include "llvm/IR/Value.h"
#include "src/frontend_ir.pb.h"

/* Visitor class declarations */
class IRVisitor;

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
  virtual llvm::Value *accept(IRVisitor &visitor) = 0;
};
std::unique_ptr<IdentifierIR> deserialiseIdentifier(
    const Frontend_ir::identifier &identifier);

struct IdentifierVarIR : public IdentifierIR {
  std::string varName;
  IdentifierVarIR(const std::string &name) : varName(name) {}
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct IdentifierObjFieldIR : public IdentifierIR {
  std::string objName;
  std::string objClassName;
  int fieldIndex;
  IdentifierObjFieldIR(const Frontend_ir::identifier::_ObjField &objfield);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

/* Expression IR */

struct ExprIR {
  virtual ~ExprIR() = default;
  virtual llvm::Value *accept(IRVisitor &visitor) = 0;
};

std::unique_ptr<ExprIR> deserialiseExpr(const Frontend_ir::expr &expr);

struct ConstructorArgIR {
  int fieldIndex;
  std::unique_ptr<ExprIR> argument;
  ConstructorArgIR(const Frontend_ir::constructor_arg &constr_arg);
};

struct ExprIntegerIR : public ExprIR {
  int val;
  ExprIntegerIR(const int &i) : val(i) {}
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};
struct ExprBooleanIR : public ExprIR {
  bool val;
  ExprBooleanIR(const bool &b) : val(b) {}
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprIdentifierIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  ExprIdentifierIR(const Frontend_ir::identifier &id)
      : identifier(deserialiseIdentifier(id)) {}
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprConstructorIR : public ExprIR {
  std::string className;
  std::vector<std::unique_ptr<ConstructorArgIR>> constructorArgs;
  ExprConstructorIR(const Frontend_ir::expr::_Constructor &constr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprLetIR : public ExprIR {
  std::string varName;
  std::unique_ptr<ExprIR> boundExpr;
  ExprLetIR(const Frontend_ir::expr::_Let &letExpr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprAssignIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  std::unique_ptr<ExprIR> assignedExpr;
  ExprAssignIR(const Frontend_ir::expr::_Assign &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprConsumeIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  ExprConsumeIR(const Frontend_ir::identifier &id)
      : identifier(deserialiseIdentifier(id)){};
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprFunctionAppIR : public ExprIR {
  std::string functionName;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprFunctionAppIR(const Frontend_ir::expr::_FunctionApp &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprFinishAsyncIR : public ExprIR {
  std::vector<std::unique_ptr<std::vector<std::unique_ptr<ExprIR>>>> asyncExprs;
  std::vector<std::unique_ptr<ExprIR>> currentThreadExpr;
  ExprFinishAsyncIR(const Frontend_ir::expr::_FinishAsync &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprIfElseIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> thenExpr;
  std::vector<std::unique_ptr<ExprIR>> elseExpr;
  ExprIfElseIR(const Frontend_ir::expr::_IfElse &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprWhileLoopIR : public ExprIR {
  std::unique_ptr<ExprIR> condExpr;
  std::vector<std::unique_ptr<ExprIR>> loopExpr;
  ExprWhileLoopIR(const Frontend_ir::expr::_WhileLoop &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprBinOpIR : public ExprIR {
  enum BinOp op;
  std::unique_ptr<ExprIR> expr1;
  std::unique_ptr<ExprIR> expr2;
  ExprBinOpIR(const Frontend_ir::expr::_BinOp &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprUnOpIR : public ExprIR {
  enum UnOp op;
  std::unique_ptr<ExprIR> expr;
  ExprUnOpIR(const Frontend_ir::expr::_UnOp &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};
