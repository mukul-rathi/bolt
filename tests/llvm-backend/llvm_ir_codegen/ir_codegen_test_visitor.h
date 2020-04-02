#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

class IRCodegenTestVisitor : public IRCodegenVisitor {
 public:
  bool isFunctionPresent(std::string functionName);
};
