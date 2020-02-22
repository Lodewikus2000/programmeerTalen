#!/bin/bash

# Main.java arguments
VERBOSE=1
NR_ITEM=100
DIMENSIONS=2
SOLVER=simulated_annealing
NR_ITERATIONS=10000
NR_TO_UNPACK=3
MAX_TEMPERATURE="0 1 10 100 1000 10000"
RUNS="1 2 3 4 5"

# Filenames
DIR_NAME="sa-logs"
FILE_PREFIX="param"

if [ $# -ne 0 ]
then
  echo "Usage: `basename $0`"
  exit $E_BADARGS
fi

ALGO_ARGUMENTS="-verbose $VERBOSE -nr_items $NR_ITEM -dimensions $DIMENSIONS -solver $SOLVER -nr_iterations $NR_ITERATIONS -nr_to_unpack $NR_TO_UNPACK"

make > /dev/null

#create directory
if [ ! -e "$DIR_NAME" ]       # Check if dir exists.
then
    echo "creating $DIR_NAME..."
    mkdir $DIR_NAME
fi

for MAX_TEMP in $MAX_TEMPERATURE
    do
    for RUN in $RUNS
        do
        echo -en "\rrunning temp $MAX_TEMP, run $RUN..."
        FILE_NAME="$FILE_PREFIX-$MAX_TEMP-run-$RUN-all.dat"
        java Main $ALGO_ARGUMENTS "-max_temperature $MAX_TEMP" > $DIR_NAME/$FILE_NAME
        done
    done

# clean everything on the line
echo -e "\033[2K"
