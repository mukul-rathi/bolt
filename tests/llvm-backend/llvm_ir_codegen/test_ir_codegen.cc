#include <stdlib.h>

#include <string>

#include "gtest/gtest.h"
#include "tests/llvm-backend/llvm_ir_codegen/ir_codegen_test_visitor.h"

class IRCodegenTest : public ::testing::Test {
 protected:
  IRCodegenTestVisitor codeGenTester;
};

TEST_F(IRCodegenTest, PrintfInstantiated) {
  codeGenTester.codegenExternFunctionDeclarations();
  EXPECT_TRUE(codeGenTester.isFunctionPresent("printf"));
}

TEST_F(IRCodegenTest, MallocInstantiated) {
  codeGenTester.codegenExternFunctionDeclarations();
  EXPECT_TRUE(codeGenTester.isFunctionPresent("malloc"));
}
TEST_F(IRCodegenTest, PthreadFuncsInstantiated) {
  codeGenTester.codegenExternFunctionDeclarations();
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_create"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_join"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_equal"));
  EXPECT_TRUE(codeGenTester.isFunctionPresent("pthread_self"));
}
