#include <stdlib.h>

#include <string>

#include "gtest/gtest.h"
#include "tests/llvm-backend/llvm_ir_codegen/ir_codegen_test_visitor.h"

class IRCodegenTest : public ::testing::Test {
 protected:
  IRCodegenTestVisitor codeGenTester;
};

TEST_F(IRCodegenTest, ExternFunctionsInstantiated) {
  codeGenTester.codegenExternFunctionDeclarations();
  EXPECT_TRUE(codeGenTester.isFunctionPresent("printf"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("malloc"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_create"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_join"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_equal"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_self"));
}
