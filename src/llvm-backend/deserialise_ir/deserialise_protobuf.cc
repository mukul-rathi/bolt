#include "src/llvm-backend/deserialise_ir/deserialise_protobuf.h"

#include <stdlib.h>

#include <fstream>
#include <string>

#include "src/llvm-backend/deserialise_ir/program_ir.h"

std::unique_ptr<ProgramIR> deserialiseProtobufFile(std::string &filePath) {
  Frontend_ir::program program;
  std::fstream fileIn(filePath, std::ios::in | std::ios::binary);
  if (!fileIn) {
    throw DeserialiseProtobufException("File not found.");
  }
  if (!program.ParseFromIstream(&fileIn)) {
    throw DeserialiseProtobufException("Protobuf not deserialised from file.");
  }
  return std::unique_ptr<ProgramIR>(new ProgramIR(program));
}
