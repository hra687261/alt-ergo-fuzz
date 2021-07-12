
## Compiling:
```
make
```
---
## Running:
In afl mode:
```
afl-fuzz -t 2000 -m 100 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
```
When a crash happens : ```(aka: total crashes > 0)```

For each crash a file is created under: ```alt-ergo/test/fuzzing/crash_output/```

Containing the marshalled bug_info of the statements whose satisfiability check caused the exception to be raised either by making Alt-Ergo crash, or by giving an unsound response (one which is contradictory to CVC5's reponse).

---
## Bug reproduction:


By running:

```
./_build/default/test/fuzzing/rerun.exe ./test/fuzzing/crash_output/op_XXXXXXXXXX.txt
```

The exception and the statements that caused the crash are read from the file ```op_XXXXXXXXXX.txt``` in which they were written after the crash, they are printed and then the solving loop is called on it to reproduce the bug.


To rerun all of the outputs:
```
for f in ./test/fuzzing/crash_output/op_* ; do  ./_build/default/test/fuzzing/rerun.exe "$f"; done;
```


---
## Parallel execution:

To run alf-fuzz in parallel mode (using more than one core), a primary instance has to be started:
```
afl-fuzz -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -M fuzzer01  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
```
And then the secondary instances:
```
afl-fuzz -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer02  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@

afl-fuzz -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer03  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
```


---
## Quickcheck mode:

To run ```afl_fuzzing.exe``` in quickcheck mode: 
```
./_build/default/test/fuzzing/afl_fuzzing.exe
```
the workflow to reproduce the bug is the same, but in this case, the exception stack trace and the output file are printed in stdout.
