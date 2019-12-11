#!/bin/bash

# run Alcotest suite
for f in $(find ./tests/alcotest -name '*.ml'); do
    dune exec  "${f%.ml}.exe" 
done

# run expect tests
dune runtest tests/expect