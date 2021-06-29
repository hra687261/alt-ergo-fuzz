#!/bin/bash

for i in test/fuzzing/crash_output/timeout* 
do 
  opf=$(echo "$i" | awk -F'[/.]' '{print $1 "/" $2 "/aefiles/" $4 "." $5 ".ae"}')
  #_build/default/test/fuzzing/to_input_format.exe $i $opf 

  if [ -f "_build/default/test/fuzzing/to_input_format.exe" ]; then
    _build/default/test/fuzzing/to_input_format.exe $i $opf 
  else 
    make 
    _build/default/test/fuzzing/to_input_format.exe $i $opf 
  fi
done

