#!/bin/bash

if [[ $1 =~ ^[0-9]+$ ]]; then t=$1; else t=10000; fi
if [[ $2 =~ ^[0-9]+$ ]]; then m=$2; else m=500; fi

afl-fuzz -t $t -m $m -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing_cached.exe @@ > ./test/fuzzing/fuzz_output/tmp0.txt 2>&1 & echo $! > ./test/fuzzing/ids.txt 
