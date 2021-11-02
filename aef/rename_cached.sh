#!/bin/bash

i=$1
for f in aef/store/*/*.txt
do
  i=$(($i + 1))
  opf=$(echo "$f" | awk -F'[/._]' '{print "aef/store/" $5 "_" i "." $8}' i=$i)
  mv $f $opf
done
