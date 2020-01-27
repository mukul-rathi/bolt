
#include <stdlib.h>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "src/llvm-backend/deserialise_ir/deserialise_protobuf.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);
  std::unique_ptr<ProgramIR> programIR =
      protobufToIR(deserialiseProtobufFile(filePath));
  IRCodegenVisitor codeGen;
  codeGen.codegenProgram(*programIR);
  codeGen.configureTarget();
  codeGen.dumpLLVMIR();
  return 0;
}
