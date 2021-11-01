#!/bin/bash

for f in ./aef/store/[uoit]*
do
  echo "#####################################"
  ./_build/default/aef/rerun.exe "$f"
  echo ""
done