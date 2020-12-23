#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

llvm::Type *IRCodegenVisitor::codegen(const TypeIntIR &typeIR) {
  return llvm::Type::getInt32Ty(*context);
};

llvm::Type *IRCodegenVisitor::codegen(const TypeClassIR &typeIR) {
  return module->getTypeByName(llvm::StringRef(typeIR.className))
      ->getPointerTo();
  ;
};

llvm::Type *IRCodegenVisitor::codegen(const TypeVoidIR &typeIR) {
  return llvm::Type::getVoidTy(*context);
};

llvm::Type *IRCodegenVisitor::codegen(const TypeBoolIR &typeIR) {
  return llvm::Type::getInt1Ty(*context);  // bools are 1-bit integers
};
