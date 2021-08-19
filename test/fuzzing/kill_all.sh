#!/bin/bash

for i in $(cat ids.txt); do
    kill -HUP $i
done
