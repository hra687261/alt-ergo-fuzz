#!/bin/bash

#Runs afl-fuzz

if [[ $1 =~ ^[0-9]+$ ]]; then t=$1; else t=10000; fi
if [[ $2 =~ ^[0-9]+$ ]]; then m=$2; else m=500; fi

gnome-terminal -- bash -c "afl-fuzz -t $t -m $m -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe @@; exec bash"

#afl-fuzz -t 5000 -m 500 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe @@ > tmp 2>&1 &
