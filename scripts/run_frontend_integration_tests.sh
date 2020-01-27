#!/bin/bash

# normal output

for f in $(find ./tests/frontend/integration -name '*.bolt'); do # run through program test_suite
    OUT_FILE="${f%.bolt}.out" #get output file path
    dune exec -- src/frontend/main.exe $f > "${OUT_FILE}" 2>&1  #write stdout and stderr to output file
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


# output for each of the flags 
FLAGS=("-print-parsed-ast" "-print-typed-ast" "-print-desugared-ast" "-print-frontend-ir" "-check-data-races" )

for ((i=0; i<${#FLAGS[@]}; i++)); do # go through array of tests 
  for f in $(find ./tests/frontend/integration -name '*.bolt'); do # run through program test_suite
    PROGRAM_FILE=$(basename $f) # get file name from path
    OUT_FILE="./tests/frontend/integration/${PROGRAM_FILE%%.*}${FLAGS[$i]}.out" #get output file path
    dune exec -- src/frontend/main.exe $f "${FLAGS[$i]}" > "${OUT_FILE}" 2>&1  #write stdout and stderr to output file
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
done 

# test invalid arguments for main
for f in $(find ./tests/frontend/integration/invalid_programs ! -name '*.out*'); do
  PROGRAM_FILE=$(basename $f) # get file name from path
  OUT_FILE="./tests/frontend/integration/invalid_programs/${PROGRAM_FILE%%.*}.out" #get output file path
  dune exec -- src/frontend/main.exe $f > "${OUT_FILE}" 2>&1  #write stdout and stderr to output file
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
 
