#!/bin/bash

if [ $# -ne 1 ];
then 
  echo "arg1 = $1"
  echo "Expects one argument: filepath to bis"
  exit 1
fi

> rc_tmp1
> rc_tmp2

./_build/default/aef/rerun_all_cached.exe $1 > rc_tmp1 2>&1
i=0
while true
do
  tmp=$(./_build/default/aef/rerun_one_cached.exe $1 $i 2>&1)
  resp=$? 
  if [[ $resp -eq 123 ]]
  then 
    exit 1
  else 
    echo "$tmp" >> rc_tmp2
  fi
  i=$(($i + 1)) 
done
