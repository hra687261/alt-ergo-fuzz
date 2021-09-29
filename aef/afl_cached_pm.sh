#!/bin/bash

#Runs afl-fuzz in parallel mode using the nomber of cores given by the first argument

if [[ $1 =~ ^[0-9]+$ ]]; then n=$1; else n=0; fi
if [[ $2 =~ ^[0-9]+$ ]]; then t=$2; else t=10000; fi
if [[ $3 =~ ^[0-9]+$ ]]; then m=$3; else m=500; fi

gnome-terminal -- bash -c "afl-fuzz -t $t -m $m -i ./aef/input/ -o ./aef/output/sync_dir/ -M fuzzer0  ./_build/default/aef/afl_fuzzing_cached.exe @@; exec bash"

for ((i = 1; i < $n; i++ )); do
  gnome-terminal -- bash -c "afl-fuzz -t $t -m $m -i ./aef/input/ -o ./aef/output/sync_dir/ -S fuzzer$i ./_build/default/aef/afl_fuzzing_cached.exe @@; exec bash"
done
