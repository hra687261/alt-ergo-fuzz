
## Compiling:
```
make
```
---
## Running:
In afl mode:
```
afl-fuzz -t 20000 -m 75 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
```
When a crash happens : ```(aka: total crashes > 0)```

For each crash a file is created under: ```alt-ergo/test/fuzzing/crash_output/```

Containing the marshalled pair (exception, ast) which are the raised exception that caused the crash and the ast whose solving causes the exception to be raised.

---
## Bug reproduction:


By running:

```
./_build/default/test/fuzzing/rerun.exe ./test/fuzzing/crash_output/op_XXXXXXXXXX.txt
```

The exception and the ast that caused the crash are read from the file ```op_XXXXXXXXXX.txt``` in which they were written after the crash, they are printed (the ast is printed before and after it's translation to an Alt-Ergo expr) and then the solving loop is called on it to reproduce the crash.


To rerun all of the outputs:
```
for f in ./test/fuzzing/crash_output/op_* ; do  ./_build/default/test/fuzzing/rerun.exe "$f"; done;
```


---
## Example:

You can test this by uncommenting the ```-> assert false``` in the lines 268 and 298 of the file ```alt-ergo/src/lib/reasoners/arith.ml```, this will introduce a bug that can be found by afl-fuzz pretty easily.


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
