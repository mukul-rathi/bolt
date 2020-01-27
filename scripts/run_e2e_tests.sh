#!/bin/bash

make pre-build
# normal output

for f in $(find ./tests/e2e -name '*.bolt'); do # run through program test_suite
    PROGRAM_FILE=$(basename $f) # get file name from path
    OUT_FILE="${f%.bolt}.ll" #get output file path
    dune exec -- src/frontend/main.exe $f 
    bazel-bin/src/llvm-backend/main "${f%.bolt}.ir" > "${f%.bolt}.ll"
    if [ -f "${OUT_FILE}.expected" ]; then # if we have expected output already
      diff "${OUT_FILE}" "${OUT_FILE}.expected" # compare output against expected output
      is_diff=$?
      if [ $is_diff -eq 1 ]; then 
        if [ "$1" == "--save" ]; then # if we want to save this output as the expected one for regression tests
          mv "${OUT_FILE}" "${OUT_FILE}.expected"
        else
          echo "Regression tests failed."
          echo $f
          exit 1 #test failed
        fi
      fi
    else
    # create expected output for regression tests in future
      cp "${OUT_FILE}" "${OUT_FILE}.expected"
    fi
  done