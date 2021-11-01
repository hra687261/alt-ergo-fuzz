
## Compiling:
```
make
```
---
## Running:
In afl mode:
```
afl-fuzz -t 2000 -m 250 -i ./aef/input/ -o ./aef/output/ ./_build/default/aef/main.exe @@
```
When a crash happens : ```(aka: total crashes > 0)```

For each crash a file is created under: ```alt-ergo/aef/store/```

Containing the marshalled bug_info of the statements whose satisfiability check caused the exception to be raised either by making Alt-Ergo crash, or by giving an unsound response (one which is contradictory to CVC5's reponse).

---
## Bug reproduction:


By running:

```
./_build/default/aef/rerun.exe ./aef/store/{sym}{num}_XXXXXXXXXX.txt
```
Where {sym} is one of the following symbols:
  - i (internal crash)
  - u (unsoundness)
  - t (timeout)
  - o (other)

And {num} is the id of the crash

The exception and the statements that caused the crash are read from the file ```op_XXXXXXXXXX.txt``` in which they were written after the crash, they are printed and then the solving loop is called on it to reproduce the bug.


To rerun all of the outputs:
```
for f in ./aef/store/[uiot]* ; do  ./_build/default/aef/rerun.exe "$f"; done;
```


---
## Parallel execution:

To run alf-fuzz in parallel mode (using more than one core), a primary instance has to be started:
```
afl-fuzz -t 2000 -m 250 -i ./aef/input/ -o ./aef/output/sync_dir/ -M fuzzer01  ./_build/default/aef/main.exe  @@
```
And then the secondary instances:
```
afl-fuzz -t 2000 -m 250 -i ./aef/input/ -o ./aef/output/sync_dir/ -S fuzzer02  ./_build/default/aef/main.exe  @@

afl-fuzz -t 2000 -m 250 -i ./aef/input/ -o ./aef/output/sync_dir/ -S fuzzer03  ./_build/default/aef/main.exe  @@
```


---
## Quickcheck mode:

To run ```main.exe``` in quickcheck mode:
```
./_build/default/aef/main.exe
```
the workflow to reproduce the bug is the same, but in this case, the exception stack trace and the output file are printed in stdout.
