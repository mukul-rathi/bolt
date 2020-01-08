#pragma once
#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/type_ir.h"

struct ClassIR {
  std::string className;
  std::vector<std::unique_ptr<TypeIR>> fields;
};