#pragma once
#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
struct TypeIR {
  virtual ~TypeIR() = default;
};

std::unique_ptr<TypeIR> deserialiseType(const Frontend_ir::type_expr &typeExpr);

struct TypeIntIR : public TypeIR {};

struct TypeClassIR : public TypeIR {
  std::string className;

  TypeClassIR(const std::string &name) : className(name) {}
};

struct TypeVoidIR : public TypeIR {};

struct TypeBoolIR : public TypeIR {};