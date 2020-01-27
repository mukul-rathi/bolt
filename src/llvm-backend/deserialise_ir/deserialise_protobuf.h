#pragma once
#include <stdlib.h>

#include <fstream>
#include <string>

#include "src/llvm-backend/deserialise_ir/program_ir.h"

Frontend_ir::program deserialiseProtobufFile(std::string &filePath);
std::unique_ptr<ProgramIR> protobufToIR(const Frontend_ir::program &program);

class DeserialiseProtobufException : public std::exception {
  std::string errorMessage;

 public:
  DeserialiseProtobufException(const char msg[]) {
    std::string errorMessage(msg);
  }
  const char *what() const throw() { return errorMessage.c_str(); }
};
