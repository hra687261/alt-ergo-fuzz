#!/bin/bash

for f in ./aef/crash_output/[uoit]*  
do  
  echo "#####################################"
  ./_build/default/aef/rerun.exe "$f"
  echo "" 
done