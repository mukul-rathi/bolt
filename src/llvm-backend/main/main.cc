#include "src/llvm-backend/lib/hello-world.h"
#include <iostream>

int main(int argc, char **argv)
{
  print_hello_world();
  std::cout << "!" << std::endl;
  return 0;
}
