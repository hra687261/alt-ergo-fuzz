#!/bin/bash

if [ $# -ne 1 ];
then 
  echo "$1"
  echo "Expects one argument representing the output language : 
  [alt-ergo | ae] | [smt-lib2 | sl2]
  Uses smt-lib (Version 2.6)"
  exit 1
fi 


if [ "$1" = "alt-ergo" ] || [ "$1" = "ae" ];
then 
  ext=".ae"
elif [ "$1" = "smt-lib2" ] || [ "$1" = "sl2" ];
then 
  ext=".smt2"
else 
  echo "$1"
  echo "The argument must be one of the following: 
  [alt-ergo | ae] | [smt-lib2 | sl2]
  Uses smt-lib (Version 2.6)"
  exit 1
fi 

if [ ! -e "_build/default/test/fuzzing/translate_and_write.exe" ];
then
  make
fi

for i in test/fuzzing/crash_output/crash* 
do 
  opf=$(echo "$i" | awk -F'[/.]' '{print $1 "/" $2 "/trfiles/" $4 "." $5}')
  opf="${opf}${ext}"
  ./_build/default/test/fuzzing/translate_and_write.exe $1 $i $opf 
done

