#!/bin/bash

#Runs afl-fuzz

if [[ $1 =~ ^[0-9]+$ ]]; then t=$1; else t=10000; fi
if [[ $2 =~ ^[0-9]+$ ]]; then m=$2; else m=500; fi

gnome-terminal -- bash -c "afl-fuzz -t $t -m $m -i ./aef/input/ -o ./aef/output/ ./_build/default/aef/afl_fuzzing.exe @@; exec bash"

#afl-fuzz -t 5000 -m 500 -i ./aef/input/ -o ./aef/output/ ./_build/default/aef/afl_fuzzing.exe @@ > tmp 2>&1 &
