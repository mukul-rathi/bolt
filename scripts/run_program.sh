#!/bin/bash

if [ $# -eq 0 ]; then
  dune exec -- src/main.exe -help
else
  dune exec -- src/main.exe $*
fi