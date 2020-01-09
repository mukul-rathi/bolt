#pragma once
#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"

/* Visitor class declarations */
class TypeIRVisitor;

struct TypeIR {
  virtual ~TypeIR() = default;
  virtual void accept(TypeIRVisitor &visitor) = 0;
};

std::unique_ptr<TypeIR> deserialiseType(const Frontend_ir::type_expr &typeExpr);

struct TypeIntIR : public TypeIR {
  virtual void accept(TypeIRVisitor &visitor) override;
};

struct TypeClassIR : public TypeIR {
  std::string className;

  TypeClassIR(const std::string &name) : className(name) {}
  virtual void accept(TypeIRVisitor &visitor) override;
};

struct TypeVoidIR : public TypeIR {
  virtual void accept(TypeIRVisitor &visitor) override;
};

struct TypeBoolIR : public TypeIR {
  virtual void accept(TypeIRVisitor &visitor) override;
};