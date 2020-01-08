#include "src/llvm-backend/deserialise_ir/program_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "src/llvm-backend/deserialise_ir/class_ir.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/function_ir.h"

ProgramIR::ProgramIR(const Frontend_ir::program &program) {
  for (int i = 0; i < program.prog()._0_size(); i++) {
    Frontend_ir::class_defn cls = program.prog()._0(i);
    classDefns.push_back(std::unique_ptr<ClassIR>(new ClassIR(cls)));
  }
  for (int i = 0; i < program.prog()._1_size(); i++) {
    Frontend_ir::function_defn fn = program.prog()._1(i);
    functionDefns.push_back(std::unique_ptr<FunctionIR>(new FunctionIR(fn)));
  }
  for (int i = 0; i < program.prog()._2_size(); i++) {
    Frontend_ir::expr expr = program.prog()._2(i);
    mainExpr.push_back(deserialiseExpr(expr));
  }
};