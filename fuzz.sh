#!/bin/bash

#Runs afl-fuzz

TERM=gnome-terminal

DIR=$(dirname "$BASH_SOURCE")
INPUTDIR="$DIR"/input/
OUTPUTDIR="$DIR"/output/
EXEC="$DIR"/../_build/default/aef/alt_ergo_fuzz.exe

PARAMS=""
NBCORES=0
TIMEOUT=10000
MEMLIMIT=500

TOFILES=false
COPF=$DIR/fuzz_output/fop
IDF=$DIR/ids.txt
SYNC_DIR=$DIR/output/sync_dir/

while (("$#")); do
  case "$1" in
  -tf | --to-files)
    TOFILES=true
    shift
    ;;
  -pm | --parallel-mode)
    if [ -n "$2" ] && [ ${2:0:1} != "-" ] && [ $2 =~ ^[0-9]+$ ]; then
      NBCORES=$2
      shift 2
    else
      echo "Error: Expected the number of cores after \"$1\"" >&2
      exit 1
    fi
    ;;
  -t | --timeout)
    if [ -n "$2" ] && [ ${2:0:1} != "-" ] && [ $2 =~ ^[0-9]+$ ]; then
      TIMEOUT=$2
      shift 2
    else
      echo "Error: Expected the timeout in milliseconds after \"$1\"" >&2
      exit 1
    fi
    ;;
  -m | --memory)
    if [ -n "$2" ] && [ ${2:0:1} != "-" ] && [ $2 =~ ^[0-9]+$ ]; then
      MEMLIMIT=$2
      shift 2
    else
      echo "Error: Expected the memory limit for child processes in megs after \"$1\"" >&2
      exit 1
    fi
    ;;
  -* | --*=)
    echo "Error: Unsupported flag \"$1\"" >&2
    exit 1
    ;;
  *)
    shift
    ;;
  esac
done

if $TOFILES; then
  if [[ ("$NBCORES" > 1) ]]; then
    (afl-fuzz -t $TIMEOUT -m $MEMLIMIT -i $INPUTDIR -o $SYNC_DIR -M fuzzer0 $EXEC @@) >"$COPF"0.txt 2>&1 &
    echo $! >$IDF
    for ((i = 1; i < $NBCORES; i++)); do
      afl-fuzz -t $TIMEOUT -m $MEMLIMIT -i $INPUTDIR -o $SYNC_DIR -S fuzzer$i $EXEC @@ >"$COPF"$i.txt 2>&1 &
      echo $! >>$IDF
    done
  else
    afl-fuzz -t $TIMEOUT -m $MEMLIMIT -i $INPUTDIR -o ./aef/output/ $EXEC @@ >"$COPF"0.txt 2>&1 &
    echo $! >$IDF
  fi
else
  if [[ ("$NBCORES" > 1) ]]; then
    $TERM -- bash -c "afl-fuzz -t $TIMEOUT -m $MEMLIMIT -i $INPUTDIR -o $SYNC_DIR -M fuzzer0 $EXEC @@; exec bash"
    for ((i = 1; i < $NBCORES; i++)); do
      $TERM -- bash -c "afl-fuzz -t $TIMEOUT -m $MEMLIMIT -i $INPUTDIR -o $SYNC_DIR -S fuzzer$i $EXEC @@; exec bash"
    done
  else
    $TERM -- bash -c "afl-fuzz -t $TIMEOUT -m $MEMLIMIT -i $INPUTDIR -o $OUTPUTDIR $EXEC @@; exec bash"
  fi
fi
