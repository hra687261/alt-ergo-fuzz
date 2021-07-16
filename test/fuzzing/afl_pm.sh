#!/bin/bash

#Runs afl-fuzz in parallel mode

if [[ $1 =~ ^[0-9]+$ ]] ; then
  n=$1 
else 
  n=0
fi

gnome-terminal -- bash -c 'afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -M fuzzer1  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@; exec bash'

for ((i = 2; i <= $n; i++ )); do
  gnome-terminal -- bash -c "afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer$i ./_build/default/test/fuzzing/afl_fuzzing.exe  @@; exec bash"
done

