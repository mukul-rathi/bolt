#include "src/llvm-backend/deserialise_ir/type_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "llvm/IR/Type.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/ir_visitor.h"

std::unique_ptr<TypeIR> deserialiseType(
    const Frontend_ir::type_expr &typeExpr) {
  switch (typeExpr.tag()) {
    case Frontend_ir::type_expr__tag_TEInt_tag:
      return std::unique_ptr<TypeIR>(new TypeIntIR());
    case Frontend_ir::type_expr__tag_TEClass_tag:
      return std::unique_ptr<TypeIR>(new TypeClassIR(typeExpr.teclass()));
    case Frontend_ir::type_expr__tag_TEVoid_tag:
      return std::unique_ptr<TypeIR>(new TypeVoidIR());

    case Frontend_ir::type_expr__tag_TEBool_tag:
      return std::unique_ptr<TypeIR>(new TypeBoolIR());
  }
}

llvm::Type *TypeIntIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
llvm::Type *TypeClassIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
llvm::Type *TypeVoidIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
llvm::Type *TypeBoolIR::accept(IRVisitor &visitor) { visitor.codegen(*this); }
