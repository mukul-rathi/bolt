#pragma once
#include <stdlib.h>

#include <string>

#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/function_ir.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

class IRVisitor {
 public:
  virtual void codegen(const IdentifierVarIR &var) = 0;
  virtual void codegen(const IdentifierObjFieldIR &objField) = 0;

  virtual void codegen(const ExprUnitIR &expr) = 0;
  virtual void codegen(const ExprIntegerIR &expr) = 0;
  virtual void codegen(const ExprBooleanIR &expr) = 0;
  virtual void codegen(const ExprIdentifierIR &expr) = 0;
  virtual void codegen(const ExprConstructorIR &expr) = 0;
  virtual void codegen(const ExprLetIR &expr) = 0;
  virtual void codegen(const ExprAssignIR &expr) = 0;
  virtual void codegen(const ExprConsumeIR &expr) = 0;
  virtual void codegen(const ExprFunctionAppIR &expr) = 0;
  virtual void codegen(const ExprFinishAsyncIR &expr) = 0;
  virtual void codegen(const ExprIfElseIR &expr) = 0;
  virtual void codegen(const ExprWhileLoopIR &expr) = 0;
  virtual void codegen(const ExprBinOpIR &expr) = 0;
  virtual void codegen(const ExprUnOpIR &expr) = 0;

  virtual void codegen(const TypeIntIR &typeIR) = 0;
  virtual void codegen(const TypeClassIR &typeIR) = 0;
  virtual void codegen(const TypeVoidIR &typeIR) = 0;
  virtual void codegen(const TypeBoolIR &typeIR) = 0;

  virtual void codegen(const ParameterParamIR &paramIR) = 0;
  virtual void codegen(const ParameterVoidIR &paramIR) = 0;
};
