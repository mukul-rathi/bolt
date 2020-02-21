#pragma once
#include <stdlib.h>

#include <string>

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/function_ir.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

class IRVisitor {
 public:
  virtual llvm::Value *codegen(const IdentifierVarIR &var) = 0;
  virtual llvm::Value *codegen(const IdentifierObjFieldIR &objField) = 0;

  virtual llvm::Value *codegen(const ExprIntegerIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBooleanIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprIdentifierIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprConstructorIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprLetIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprAssignIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprConsumeIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprMethodAppIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprFinishAsyncIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprIfElseIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprWhileLoopIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprUnOpIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprPrintfIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprBlockIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprLockIR &expr) = 0;
  virtual llvm::Value *codegen(const ExprUnlockIR &expr) = 0;

  virtual llvm::Type *codegen(const TypeIntIR &typeIR) = 0;
  virtual llvm::Type *codegen(const TypeClassIR &typeIR) = 0;
  virtual llvm::Type *codegen(const TypeVoidIR &typeIR) = 0;
  virtual llvm::Type *codegen(const TypeBoolIR &typeIR) = 0;
};
