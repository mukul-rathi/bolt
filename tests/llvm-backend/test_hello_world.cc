#include "gtest/gtest.h"
#include "src/llvm-backend/lib/hello-world.h"

TEST(TestHelloWorld, GetHelloWorld) { EXPECT_EQ(get_hello_world(), ""); }
