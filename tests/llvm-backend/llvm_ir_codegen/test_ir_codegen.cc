#include <stdlib.h>

#include <string>

#include "gtest/gtest.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

class IRCodegenVisitorTest : public ::testing::Test {
 protected:
  IRCodegenVisitor codeGen;
};

TEST_F(IRCodegenVisitorTest, EmptyModule) {
  ASSERT_STREQ(codeGen.dumpLLVMIRToString().c_str(),
               "; ModuleID = 'Module'\nsource_filename = \"Module\"\n");
}
