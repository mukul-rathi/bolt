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
  std::string varName;
  virtual ~IdentifierIR() = default;
  virtual llvm::Value *accept(IRVisitor &visitor) = 0;
};
std::unique_ptr<IdentifierIR> deserialiseIdentifier(
    const Frontend_ir::identifier &identifier);

struct IdentifierVarIR : public IdentifierIR {
  IdentifierVarIR(const std::string &name);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct IdentifierObjFieldIR : public IdentifierIR {
  std::string objName;
  std::string objClassName;
  int fieldIndex;
  IdentifierObjFieldIR(const Frontend_ir::identifier::_ObjField &objfield);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

/* Lock types - have enum value = field index in object */

enum LockType { Reader = 2, Writer = 3 };

LockType deserialiseLockType(const Frontend_ir::lock_type &lockType);

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

struct AsyncExprIR {
  std::vector<std::string> freeVars;
  std::vector<std::unique_ptr<ExprIR>> exprs;
  AsyncExprIR(const Frontend_ir::async_expr &expr);
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
  bool shouldLock;
  LockType lockType;
  ExprIdentifierIR(const Frontend_ir::expr::_Identifier &expr);
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
  bool shouldLock;
  LockType lockType;
  ExprAssignIR(const Frontend_ir::expr::_Assign &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprConsumeIR : public ExprIR {
  std::unique_ptr<IdentifierIR> identifier;
  bool shouldLock;
  LockType lockType;
  ExprConsumeIR(const Frontend_ir::expr::_Consume &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprFunctionAppIR : public ExprIR {
  std::string functionName;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprFunctionAppIR(const Frontend_ir::expr::_FunctionApp &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprMethodAppIR : public ExprIR {
  std::string objName;
  std::string objStaticMethName;
  int methodIndex;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprMethodAppIR(const Frontend_ir::expr::_MethodApp &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprPrintfIR : public ExprIR {
  std::string formatStr;
  std::vector<std::unique_ptr<ExprIR>> arguments;
  ExprPrintfIR(const Frontend_ir::expr::_Printf &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprFinishAsyncIR : public ExprIR {
  std::vector<std::unique_ptr<AsyncExprIR>> asyncExprs;
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

struct ExprBlockIR : public ExprIR {
  std::vector<std::unique_ptr<ExprIR>> exprs;
  ExprBlockIR(const Frontend_ir::block_expr &expr);
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprLockIR : public ExprIR {
  std::string objName;
  LockType lockType;
  ExprLockIR(const Frontend_ir::expr::_Lock &expr);
  ExprLockIR(const std::string &name, LockType lockType)
      : objName(name), lockType(lockType){};
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};

struct ExprUnlockIR : public ExprIR {
  std::string objName;
  LockType lockType;
  ExprUnlockIR(const Frontend_ir::expr::_Unlock &expr);
  ExprUnlockIR(const std::string &name, LockType lockType)
      : objName(name), lockType(lockType){};
  virtual llvm::Value *accept(IRVisitor &visitor) override;
};