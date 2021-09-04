#!/bin/bash

for i in $(cat ./test/fuzzing/ids.txt); do
    kill -HUP $i
done
