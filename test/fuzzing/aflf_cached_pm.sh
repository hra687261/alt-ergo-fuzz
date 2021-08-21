#!/bin/bash

#Runs afl-fuzz in parallel mode using the number of cores given by the first argument
#The output of afl will be redirected to the files in fuzz_output/fop*.txt

if [[ $1 =~ ^[0-9]+$ ]]; then n=$1; else n=0; fi
if [[ $2 =~ ^[0-9]+$ ]]; then t=$2; else t=10000; fi
if [[ $3 =~ ^[0-9]+$ ]]; then m=$3; else m=500; fi

(afl-fuzz -t $t -m $m -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -M fuzzer0 ./_build/default/test/fuzzing/afl_fuzzing_cached.exe @@) > ./test/fuzzing/fuzz_output/fop0.txt 2>&1 & echo $! > ./test/fuzzing/ids.txt 

for ((i = 1; i < $n; i++ )); do
  afl-fuzz -t $t -m $m -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer$i ./_build/default/test/fuzzing/afl_fuzzing_cached.exe @@ > ./test/fuzzing/fuzz_output/fop$i.txt 2>&1 & echo $! >> ./test/fuzzing/ids.txt
done
