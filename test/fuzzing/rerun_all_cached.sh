#!/bin/bash

if [ $# -ne 1 ];
then 
  echo "arg1 = $1"
  echo "Expects one argument: filepath to bis"
  exit 1
fi

./_build/default/test/fuzzing/rerun_all_cached.exe $1 > rc_tmp1 2>&1
truncate -s 0 rc_tmp2
error="Fatal error: exception Failure(\"nth\")"
i=0
while true
do
  tmp=$(./_build/default/test/fuzzing/rerun_one_cached.exe $1 $i 2>&1)
  if [[ $tmp == $error ]]
  then 
    exit 1
  else 
    echo "$tmp" >> rc_tmp2
  fi
  i=$(($i + 1)) 
done
