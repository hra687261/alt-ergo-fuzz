#!/bin/bash

if [[ $1 =~ ^[0-9]+$ ]]; then t=$1; else t=10000; fi
if [[ $2 =~ ^[0-9]+$ ]]; then m=$2; else m=500; fi

afl-fuzz -t $t -m $m -i ./aef/input/ -o ./aef/output/ ./_build/default/aef/afl_fuzzing.exe @@ > ./aef/fuzz_output/tmp0.txt 2>&1 & echo $! > ./aef/ids.txt 
