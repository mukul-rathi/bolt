
#include "src/llvm-backend/deserialise_ir/class_ir.h"

#include <stdlib.h>

#include <string>
#include <vector>

#include "src/frontend_ir.pb.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

ClassIR::ClassIR(const Frontend_ir::class_defn &classDefn) {
  className = classDefn.tclass()._0();
  for (int i = 0; i < classDefn.tclass()._1_size(); i++) {
    fields.push_back(deserialiseType(classDefn.tclass()._1(i)));
  }
}
