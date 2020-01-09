
#include "src/llvm-backend/deserialise_ir/deserialise_protobuf.h"
#include "src/llvm-backend/deserialise_ir/program_ir.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);
  (deserialiseProtobufFile(filePath)).PrintDebugString();
  return 0;
}
