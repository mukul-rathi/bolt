#!/bin/bash

if [ $# -eq 0 ]; then
 dune exec -- src/frontend/main.exe -help
else
  dune exec -- src/frontend/main.exe $*
fi