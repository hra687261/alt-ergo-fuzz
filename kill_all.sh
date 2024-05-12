#!/bin/bash

for i in $(cat ./aef/ids.txt); do
    kill -HUP $i
done
