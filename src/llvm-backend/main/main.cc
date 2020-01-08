
#include "src/llvm-backend/deserialise_ir/deserialise_protobuf.h"
#include "src/llvm-backend/deserialise_ir/program_ir.h"

int main(int argc, char **argv) {
  std::string foo(argv[1]);
  deserialiseProtobufFile(foo);
  return 0;
}
