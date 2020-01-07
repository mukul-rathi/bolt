#pragma once
#include <string>
#include <stdlib.h>
#include <vector>
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

class ParamIR {
  TypeIR paramType;
  std::string paramName;
};

class FunctionIR {
  std::string functionName;
  TypeIR returnType;
  std::vector<std::unique_ptr<ParamIR>> params;
  std::vector<std::unique_ptr<ExprIR>> bodyExpr;
};