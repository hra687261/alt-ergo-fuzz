#!/bin/bash

#Runs afl-fuzz in parallel mode using the nomber of cores given by the first argument

if [[ $1 =~ ^[0-9]+$ ]] ; then
  n=$1 
else 
  n=0
fi

gnome-terminal -- bash -c 'afl-fuzz -t 5000 -m 300 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -M fuzzer1  ./_build/default/test/fuzzing/afl_fuzzing_cached.exe @@; exec bash'

for ((i = 2; i <= $n; i++ )); do
  gnome-terminal -- bash -c "afl-fuzz -t 5000 -m 300 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer$i ./_build/default/test/fuzzing/afl_fuzzing_cached.exe @@; exec bash"
done
