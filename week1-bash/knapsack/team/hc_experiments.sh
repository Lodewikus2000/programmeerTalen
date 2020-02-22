#!/bin/bash

# Main.java arguments
VERBOSE=1
NR_ITEM=100
DIMENSIONS=2
SOLVER=hill_climbing
NR_ITERATIONS=10000
NR_TO_UNPACK="1 2 3 4 5 6"
RUNS="1 2 3 4 5"

# Filenames
DIR_NAME="hc-logs"
FILE_PREFIX="param"

if [ $# -ne 0 ]
then
  echo "Usage: `basename $0`"
  exit $E_BADARGS
fi

ALGO_ARGUMENTS="-verbose $VERBOSE -nr_items $NR_ITEM -dimensions $DIMENSIONS -solver $SOLVER -nr_iterations $NR_ITERATIONS"

make > /dev/null

#create directory
if [ ! -e "$DIR_NAME" ]       # Check if dir exists.
then
    echo "creating $DIR_NAME..."
    mkdir $DIR_NAME
fi

for NR in $NR_TO_UNPACK
    do
    for RUN in $RUNS
        do
        echo -en "\rrunning number to unpack $NR, run $RUN..."
        FILE_NAME="$FILE_PREFIX-$NR-run-$RUN-all.dat"
        java Main $ALGO_ARGUMENTS "-nr_to_unpack $NR" > $DIR_NAME/$FILE_NAME
        done
    done

# clean everything on the line
echo -e "\033[2K"
