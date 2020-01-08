#!/bin/bash
make pre-build
if [ $# -eq 0 ]; then
 dune exec -- src/frontend/main.exe -help
else
  dune exec -- src/frontend/main.exe $*
  bazel-bin/src/llvm-backend/main/main "${1%.bolt}.compiled"
fi
