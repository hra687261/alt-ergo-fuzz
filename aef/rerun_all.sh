#!/bin/bash

for f in ./aef/store/*/*.txt
do
  echo "#####################################"
  ./_build/default/aef/rerun.exe "$f"
  echo ""
done