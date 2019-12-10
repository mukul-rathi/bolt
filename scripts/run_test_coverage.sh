#!/bin/bash

# run Alcotest suite
for f in $(find ./tests -name '*.ml'); do
    dune exec  "${f%.ml}.exe" 
done

# run integration / E2E on test-suite of programs
bash  scripts/run_integration_E2E_tests.sh --all
