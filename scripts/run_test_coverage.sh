#!/bin/bash

# run Alcotest suite
for f in $(find ./tests/frontend/alcotest -name '*.ml'); do
    dune exec  "${f%.ml}.exe" 
done

# run expect tests
dune runtest tests/frontend/expect

# run e2e tests
make pre-build
bash scripts/run_e2e_tests.sh