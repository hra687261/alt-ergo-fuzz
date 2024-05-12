
Alt-Ergo-Fuzz: A fuzzer for the Alt-Ergo SMT solver.

### Requirements:
  - [CVC5](https://github.com/cvc5/cvc5).
  - [AFL](https://github.com/google/AFL).
  - Alt-Ergo's compilation requirements + the [crowbar](https://github.com/stedolan/crowbar) OCaml library.

(P.S: The following commands assume that you're placed at the root of the repo)

# Compiling:
```
make
./aef/init.sh #to generate necessary folders/files for the fuzzer
```
---
# Running:
To run the fuzzer:
```
./aef/fuzz.sh
```
Optional command line arguments:
```
  -tf, --to-files
    Doesn't open a terminal but redirects the output to a file in
    "./fuzz_output/fop{num}.txt".

  -pm NBCORES, --parallel-mode=NBCORES
    Runs the fuzzer in the parallel mode using NBCORES cores (if they are
    availabe).

  -t TIMEOUT, --timeout=TIMEOUT
    Sets the timeout of every run to TIMEOUT (in milliseconds).

  -m MEMLIMIT, --memory=MEMLIMIT
    Sets the memory consumption limit of every run to MEMLIMIT (in
    megabytes).

```

When a crash happens : ```(aka: total crashes > 0)```

For each crash a file is created under: ```./aef/store/{foldername}/```

Containing information about what caused the crash in a marshalled file.

```{foldername}``` is the name of the subfolder under which the crash file is stored, and it represents the type of error that caused the crash, it can be one of the following: ```[internalcrash|outofmemory|stackoverflow|timeout|other]```.

---

# Bug reproduction:

It can be done by running:

```
./_build/default/aef/main.exe [-r|--rerun] [-i|--input] {ipf}
```
Which reruns the solvers on the list of SMT statements that caused the crash.

If the option ```[-v|--verbose]``` is provided, then it will print the original answers and the exception that was raised during the crash, as well as the new answers.

# Translation of crash files:

The list of SMT statements that caused some crash can be translated to the
SMT-LIB2 standard or Alt-Ergo's native langauge and printed into a file by
running:
```
./_build/default/aef/main.exe [-t|--translate] [alt-ergo|smt-lib2] [-i|--input] {ipf} [-o|--output] {opf[.ae|.smt2]}
```
