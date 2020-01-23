#pragma once
#include <string>
#include <vector>

#include "llvm/IR/Type.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"

/* Visitor class declarations */
class IRVisitor;

struct TypeIR {
  virtual ~TypeIR() = default;
  virtual llvm::Type *accept(IRVisitor &visitor) = 0;
};

std::unique_ptr<TypeIR> deserialiseType(const Frontend_ir::type_expr &typeExpr);

struct TypeIntIR : public TypeIR {
  virtual llvm::Type *accept(IRVisitor &visitor) override;
};

struct TypeClassIR : public TypeIR {
  std::string className;

  TypeClassIR(const std::string &name) : className(name) {}
  virtual llvm::Type *accept(IRVisitor &visitor) override;
};

struct TypeVoidIR : public TypeIR {
  virtual llvm::Type *accept(IRVisitor &visitor) override;
};

struct TypeBoolIR : public TypeIR {
  virtual llvm::Type *accept(IRVisitor &visitor) override;
};
