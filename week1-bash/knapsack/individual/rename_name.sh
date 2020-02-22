#!/bin/bash

E_BADARGS=65

if [ $# -ne 2 ]
then
  echo "Usage: `basename $0` [old-name] [new-name]"
  exit $E_BADARGS
fi

cd ..

# find all java files
FILES=$(find -name '*.java')

# iterate over files
for file in $FILES
do
    # get contents of file,
    cat $file | perl -pe "s/$1/$2/sg" > temp.txt
    mv temp.txt $file
done

exit $?
