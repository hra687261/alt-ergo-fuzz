#!/bin/bash

i=$1
for f in aef/crash_output/c* 
do 
  i=$(($i + 1))
  opf=$(echo "$f" | awk -F'[/._]' '{print "aef/crash_output/" $5 "_" i "." $8}' i=$i)
  mv $f $opf
done
