#!/bin/bash

if [ ! -e "_build/default/aef/translate_and_write.exe" ];
then
  make
fi

for i in aef/crash_output/[uito]* 
do 
  opf=$(echo "$i" | awk -F'[/.]' '{print $1 "/" $2 "/trfiles/" $4 "." $5}')
  opfae="${opf}.ae"
  opfsl2="${opf}.smt2"
  ./_build/default/aef/translate_and_write.exe $i $opfae ae 
  ./_build/default/aef/translate_and_write.exe $i $opfsl2 sl2
done