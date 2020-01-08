#pragma once
#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

struct ParameterIR {
  virtual ~ParameterIR() = default;
};

std::unique_ptr<ParameterIR> deserialiseParameter(
    const Frontend_ir::param &param);

struct ParameterParamIR : public ParameterIR {
  std::unique_ptr<TypeIR> paramType;
  std::string paramName;

  ParameterParamIR(const Frontend_ir::param::_TParam &param);
};

struct ParameterVoidIR : public ParameterIR {};

struct FunctionIR {
  std::string functionName;
  std::unique_ptr<TypeIR> returnType;
  std::vector<std::unique_ptr<ParameterIR>> params;
  std::vector<std::unique_ptr<ExprIR>> bodyExpr;

  FunctionIR(const Frontend_ir::function_defn &functionDefn);
};