#include "gtest/gtest.h"
#include "src/llvm-backend/lib/hello-world.h"

TEST(TestHelloWorld, GetHelloWorld) {
  EXPECT_EQ(get_hello_world(), "Hello World");
}
TEST(TestHelloWorld, GetHelloWorldAgain) {
  EXPECT_EQ(get_hello_world(), "Hello World");
}