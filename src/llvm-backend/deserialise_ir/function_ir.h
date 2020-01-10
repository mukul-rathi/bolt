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
  virtual ~ParameterIR() = default;
  virtual void accept(IRVisitor &visitor) = 0;
};

std::unique_ptr<ParameterIR> deserialiseParameter(
    const Frontend_ir::param &param);

struct ParameterParamIR : public ParameterIR {
  std::unique_ptr<TypeIR> paramType;
  std::string paramName;

  ParameterParamIR(const Frontend_ir::param::_TParam &param);
  virtual void accept(IRVisitor &visitor) override;
};

struct ParameterVoidIR : public ParameterIR {
  virtual void accept(IRVisitor &visitor) override;
};

struct FunctionIR {
  std::string functionName;
  std::unique_ptr<TypeIR> returnType;
  std::vector<std::unique_ptr<ParameterIR>> params;
  std::vector<std::unique_ptr<ExprIR>> bodyExpr;

  FunctionIR(const Frontend_ir::function_defn &functionDefn);
};
