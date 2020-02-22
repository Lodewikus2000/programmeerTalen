#!/bin/bash
# find_name.sh: Locates matching string in all java files.
echo

# go to upper dir a and search upper directory for java files
cd ..

FILES=$(find -name '*.java')
# count, print file name, print line number, fixed string
GREP_OPTS="-Hon"

E_BADARGS=65

if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` [search_string]"
  exit $E_BADARGS
fi

# remove for loop? -exec
for file in $FILES
do
    grep $GREP_OPTS "$1" "$file" | cut -f 1-2 -d:
done

#TO DO: awk -flafg separator
exit $?
