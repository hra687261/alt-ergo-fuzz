
afl-fuzz -t 5000 -m 500 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe @@ > ./test/fuzzing/fuzz_output/tmp0.txt 2>&1 &
echo $! > ./test/fuzzing/ids.txt 
