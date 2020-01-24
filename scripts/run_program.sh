#!/bin/bash
make pre-build
if [ $# -eq 0 ]; then
 dune exec -- src/frontend/main.exe -help
else
  dune exec -- src/frontend/main.exe $*
  bazel-bin/src/llvm-backend/main "${1%.bolt}.compiled" > "${1%.bolt}.ll"
  llc "${1%.bolt}.ll"
  clang "${1%.bolt}.s" -o "${1%.bolt}"
  ./${1%.bolt}
fi
