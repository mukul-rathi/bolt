#include <iostream>

#include "src/llvm-backend/lib/hello-world.h"

int main(int argc, char **argv) {
  std::cout << get_hello_world() << std::endl;
  return 0;
}
