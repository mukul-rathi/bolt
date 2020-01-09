
#include <stdlib.h>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "src/llvm-backend/deserialise_ir/deserialise_protobuf.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);
  protobufToIR(deserialiseProtobufFile(filePath));
  std::unique_ptr<llvm::LLVMContext> context =
      llvm::make_unique<llvm::LLVMContext>();
  llvm::IRBuilder<> builder(*context);
  std::unique_ptr<llvm::Module> module =
      llvm::make_unique<llvm::Module>("Module", *context);
  module->print(llvm::outs(), nullptr);
  return 0;
}
