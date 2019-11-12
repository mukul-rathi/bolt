#!/bin/bash
make doc	
shopt -s extglob	
rm -rf !(docs)	
mv docs/* .