
Compiling:
```
make
```
Running:

In afl mode:
```
afl-fuzz -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
```
When a crash happens : ```(aka: total crashes > 0)```

For each crash a file is created under: ```alt-ergo/test/fuzzing/crash_output/```

Containing the marshalled pair (exception, ast) which are the raised exception that caused the crash and the ast who's solving causes the exception to be raised.

By running:

```
./_build/default/test/fuzzing/rerun.exe ./test/fuzzing/crash_output/op_XXXXXXXXXX.txt
```

The exception and the ast that caused the crash are read from the file ```op_XXXXXXXXXX.txt``` in which they were written after the crash, they are printed (the ast is printed before and after it's translation to an Alt-Ergo expr) and then the solving loop is called with it to try and reproduce the crash.

You can also run it in quickcheck mode: 
```
./_build/default/test/fuzzing/afl_fuzzing.exe
```
the workflow to reproduce the bug is the same, but in this case, the exception stack trace and the output file are printed in stdout.
