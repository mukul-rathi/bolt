#include "src/llvm-backend/deserialise_ir/function_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/ir_visitor.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

std::unique_ptr<ParameterIR> deserialiseParameter(
    const Frontend_ir::param &param) {
  switch (param.tag()) {
    case Frontend_ir::param__tag_TParam_tag:
      return std::unique_ptr<ParameterIR>(new ParameterParamIR(param.tparam()));
    case Frontend_ir::param__tag_TVoid_tag:
      return std::unique_ptr<ParameterIR>(new ParameterVoidIR());
  }
}

ParameterParamIR::ParameterParamIR(const Frontend_ir::param::_TParam &param) {
  paramType = deserialiseType(param._0());
  paramName = param._1();
}
void ParameterParamIR::accept(ParameterIRVisitor &visitor) {
  visitor.codegen(*this);
}
void ParameterVoidIR::accept(ParameterIRVisitor &visitor) {
  visitor.codegen(*this);
}

FunctionIR::FunctionIR(const Frontend_ir::function_defn &functionDefn) {
  functionName = functionDefn.tfunction()._0();
  returnType = deserialiseType(functionDefn.tfunction()._1());

  for (int i = 0; i < functionDefn.tfunction()._2_size(); i++) {
    params.push_back(deserialiseParameter(functionDefn.tfunction()._2(i)));
  }

  for (int i = 0; i < functionDefn.tfunction()._3_size(); i++) {
    bodyExpr.push_back(deserialiseExpr(functionDefn.tfunction()._3(i)));
  }
}