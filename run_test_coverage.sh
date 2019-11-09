#!/bin/bash
for f in $(find ./tests/ -name '*.ml'); do
    dune exec  "${f%.ml}.exe" 
done