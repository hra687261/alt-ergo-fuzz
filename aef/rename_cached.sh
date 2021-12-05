#!/bin/bash

DIR=$(dirname "$BASH_SOURCE")

i=0

for f in $(echo $DIR/store/*/*.*_*.txt)
do
  i=$(($i + 1))
  opf=$(echo "$f" | awk -F'[/._]' '{print DIR "/store/" i "." $9}' i=$i DIR=$DIR)
  echo "$f  $opf  $i"
done
