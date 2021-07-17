#!/bin/sh

#Runs afl-fuzz

gnome-terminal -- bash -c 'afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe @@; exec bash'