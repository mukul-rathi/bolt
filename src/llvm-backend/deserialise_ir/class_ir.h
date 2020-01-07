#pragma once
#include <stdlib.h>
#include <vector>
#include <string>

#include "src/llvm-backend/deserialise_ir/type_ir.h"

class ClassIR {
  std::string className;
  std::vector<std::unique_ptr<TypeIR>> fields;
};