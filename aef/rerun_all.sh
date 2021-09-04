#!/bin/bash

for f in ./test/fuzzing/crash_output/[uoit]*  
do  
  echo "#####################################"
  ./_build/default/test/fuzzing/rerun.exe "$f"
  echo "" 
done