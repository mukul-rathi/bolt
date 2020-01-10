#pragma once
#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/class_ir.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/function_ir.h"

struct ProgramIR {
  std::vector<std::unique_ptr<ClassIR>> classDefns;
  std::vector<std::unique_ptr<FunctionIR>> functionDefns;
  std::vector<std::unique_ptr<ExprIR>> mainExpr;

  ProgramIR(const Frontend_ir::program &program);
};
