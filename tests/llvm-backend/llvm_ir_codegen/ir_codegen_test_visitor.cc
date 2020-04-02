#include "tests/llvm-backend/llvm_ir_codegen/ir_codegen_test_visitor.h"

bool IRCodegenTestVisitor::isFunctionPresent(std::string functionName) {
  return (module->getFunction(functionName) != nullptr);
}