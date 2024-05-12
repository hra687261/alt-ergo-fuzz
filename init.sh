#!/bin/bash

DIR=$(dirname "$BASH_SOURCE")

OPDIR="$DIR/output"
SDIR="$DIR/store"

ndirs="$DIR/fuzz_output $DIR/input $OPDIR $OPDIR/sync_dir
$SDIR $SDIR/internalcrash $SDIR/other $SDIR/outofmemory
$SDIR/stackoverflow $SDIR/timeout $SDIR/unsoundness
$DIR/translated_files"

for f in $ndirs; do
  if [ ! -d $f ]; then
    mkdir $f
  fi
done

echo 000000 > $DIR/input/input.txt
