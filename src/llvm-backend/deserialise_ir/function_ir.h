#pragma once
#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"
/* Visitor class declarations */
class IRVisitor;

struct ParameterIR {
  std::unique_ptr<TypeIR> paramType;
  std::string paramName;
  ParameterIR(const Frontend_ir::param::_TParam &param);
};

struct FunctionIR {
  std::string functionName;
  std::unique_ptr<TypeIR> returnType;
  std::vector<std::unique_ptr<ParameterIR>> params;
  std::vector<std::unique_ptr<ExprIR>> bodyExpr;

  FunctionIR(const Frontend_ir::function_defn &functionDefn);
};
