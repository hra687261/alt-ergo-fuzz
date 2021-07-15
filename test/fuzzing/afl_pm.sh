#!/bin/sh

#Runs afl-fuzz in parallel mode

gnome-terminal -- bash -c 'afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -M fuzzer01  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@; exec bash'

gnome-terminal -- bash -c 'afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer02  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@; exec bash'

gnome-terminal -- bash -c 'afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer03  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@; exec bash'

gnome-terminal -- bash -c 'afl-fuzz -t 2000 -m 250 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/sync_dir/ -S fuzzer04  ./_build/default/test/fuzzing/afl_fuzzing.exe  @@; exec bash'