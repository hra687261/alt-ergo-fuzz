#!/bin/bash

#Runs afl-fuzz

gnome-terminal -- bash -c 'afl-fuzz -t 5000 -m 500 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe @@; exec bash'