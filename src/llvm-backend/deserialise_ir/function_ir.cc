#include "src/llvm-backend/deserialise_ir/function_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/ir_visitor.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

ParameterIR::ParameterIR(const Frontend_ir::param::_TParam &param) {
  paramType = deserialiseType(param._0());
  paramName = param._1();
}

FunctionIR::FunctionIR(const Frontend_ir::function_defn &functionDefn) {
  functionName = functionDefn.tfunction()._0();
  returnType = deserialiseType(functionDefn.tfunction()._1());

  for (int i = 0; i < functionDefn.tfunction()._2_size(); i++) {
    params.push_back(std::unique_ptr<ParameterIR>(
        new ParameterIR(functionDefn.tfunction()._2(i).tparam())));
  }

  for (int i = 0; i < functionDefn.tfunction()._3_size(); i++) {
    bodyExpr.push_back(deserialiseExpr(functionDefn.tfunction()._3(i)));
  }
}
