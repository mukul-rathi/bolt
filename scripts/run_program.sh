#!/bin/bash
scripts/compile_program.sh $* &&  if [ -f "${1%.bolt}.ir" ]; then ./${1%.bolt} 
fi
