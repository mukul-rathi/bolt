#!/bin/bash
make build
rm -rf "${1%.bolt}.ir"
if [ $# -eq 0 ]; then
 dune exec -- src/frontend/main.exe -help
else
  (dune exec -- src/frontend/main.exe $*)
  if [ -f "${1%.bolt}.ir" ]; then
    bazel-bin/src/llvm-backend/main "${1%.bolt}.ir" > "${1%.bolt}.ll"
    clang -pthread "${1%.bolt}.ll" -o "${1%.bolt}"
    ./${1%.bolt}
  fi
fi
