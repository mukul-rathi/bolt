#!/bin/bash
make pre-build
if [ $# -eq 0 ]; then
 dune exec -- src/frontend/main.exe -help
else
  dune exec -- src/frontend/main.exe $*
  bazel-bin/src/llvm-backend/main "${1%.bolt}.ir" > "${1%.bolt}.ll"
  clang -pthread "${1%.bolt}.ll" -o "${1%.bolt}"
  ./${1%.bolt}
fi
