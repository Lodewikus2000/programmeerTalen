#!/bin/bash

DIR_NAME="hc-logs"
FILE_PREFIX="final"
FILE_EXTENSION='*.dat'

HEADER_END_STRING='Knapsack'
GREP_OPTS="-hon -m 1"
AVG_FILE_BLOT='param-*-all.dat'
CLEANED_NAMES='param-*-cleaned.dat'
FINAL_NAMES='final-*.dat'

STATISTICS='hc-statistics.dat'

if [ $# -ne 0 ]
then
  echo "Usage: `basename $0`"
  exit $E_BADARGS
fi

if [ ! -e "$DIR_NAME" ]       # Check if dir exists.
then
    echo "$DIR_NAME does not exit, please run ./hc_experiments.sh first."
    exit 1
fi

FILES=$(find $DIR_NAME -name "$AVG_FILE_BLOT" | sort -n)

for file in $FILES
    do
    # find 'Knapsack' in file, and start reading from the line below it.
    HEADER=$(($(grep $GREP_OPTS $HEADER_END_STRING $file | cut -f 1 -d:) + 2))
    tail $file --lines=+$HEADER | cut -d' ' -f1,5 | awk -F '[ :]' '{print $2" "$4}'  > $DIR_NAME/cleaned.txt

    # build new file name
    PARAM=$(echo $file | echo $(cut -d'-' -f3))
    AVG_FILE_NAME="final-$PARAM.dat"

    # find the final value in cleaned file
    tail $DIR_NAME/cleaned.txt --lines=2 | awk -F" " '{print $2}' | head -1 >> $DIR_NAME/$AVG_FILE_NAME

    # update the original file
    NEW_NAME=$(echo "$file" | cut -d "." -f 1)-cleaned.dat
    cat $DIR_NAME/cleaned.txt > $NEW_NAME
    done

# remove last temporary file
if [ -e "$DIR_NAME/cleaned.txt" ]       # Check if file exists.
then
  rm $DIR_NAME/cleaned.txt
fi

AVG_FILES=$(find $DIR_NAME -name "$FINAL_NAMES" | sort -n)
count=1
for file in $AVG_FILES
    do
    echo -e "$count param:$count\c"  >> $DIR_NAME/$STATISTICS
    awk '{{sum += $0; sumsq += ($0)^2}}
        END { printf " %f %f \n", sum/NR, sqrt((sumsq-sum^2/NR)/NR)}' $file >> $DIR_NAME/$STATISTICS
    ((count++))
    done
