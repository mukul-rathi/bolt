#!/bin/bash

if [ $# -eq 0 ]; then
 dune exec -- src/frontend/frontend_cmd_line.exe -help
else
  dune exec -- src/frontend/frontend_cmd_line.exe $*
fi