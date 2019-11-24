#!/bin/bash

# flags to determine which set of tests to run
if [ "$1" == "--all" ]; then
  TEST_DIR=("parsing" "core_type_checker" "data_race_type_checker")
  FLAGS=("-print-parsed-ast" "-print-typed-ast" "-check-data-races")
elif [ "$1" == "--parsing" ]; then
  TEST_DIR=("parsing")
  FLAGS=("-print-parsed-ast")
elif [ "$1" == "--core-type-checker" ]; then  TEST_DIR=("core_type_checker")
  FLAGS=("-print-typed-ast")
elif [ "$1" == "--data-race-type-checker" ]; then  TEST_DIR=("data_race_type_checker")
  FLAGS=("-print-typed-ast")
else 
  echo "Enter a test option:
      --all : run all tests 
      --parsing: run tests for lexer + parser
      --core-type-checker: run tests      
      --data-race-type-checker: run tests for the core type-checker
        for the data-race type-checker

      Include the second flag --save to save output as expected output (for regression tests)
"
  exit 1
fi

for ((i=0; i<${#TEST_DIR[@]}; i++)); do # go through array of tests 
  for f in $(find ./tests/programs -name '*.bolt'); do # run through program test_suite
    PROGRAM_FILE=$(basename $f) # get file name from path
    OUT_FILE="./tests/${TEST_DIR[$i]}/${PROGRAM_FILE%%.*}.out" #get output file path
    dune exec -- src/main.exe $f "${FLAGS[$i]}" > "${OUT_FILE}" 2>&1  #write stdout and stderr to output file
    if [ -f "${OUT_FILE}.expected" ]; then # if we have expected output already
      diff "${OUT_FILE}" "${OUT_FILE}.expected" # compare output against expected output
      is_diff=$?
      if [ $is_diff -eq 1 ]; then 
        if [ "$2" == "--save" ]; then # if we want to save this output as the expected one for regression tests
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
for f in $(find ./tests/programs ! -name '*.bolt'); do
  PROGRAM_FILE=$(basename $f) # get file name from path
  OUT_FILE="./tests/main_cmd_line/${PROGRAM_FILE%%.*}.out" #get output file path
  dune exec -- src/main.exe $f >> "${OUT_FILE}" 2>&1  #write stdout and stderr to output file
  if [ -f "${OUT_FILE}.expected" ]; then # if we have expected output already
    diff "${OUT_FILE}" "${OUT_FILE}.expected" # compare output against expected output
    cp "${OUT_FILE}" "${OUT_FILE}.corrected" 
    if [ "$2" == "--save" ]; then # if we want to save this output as the expected one for regression tests
      mv "${OUT_FILE}.corrected" "${OUT_FILE}.expected"
    fi
  else
  # create expected output for regression tests in future
    cp "${OUT_FILE}" "${OUT_FILE}.expected"
  fi
  rm "${OUT_FILE}" # clean-up output of this test
done
 