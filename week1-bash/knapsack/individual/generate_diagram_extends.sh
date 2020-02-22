#!/bin/bash

echo "digraph D {"
cd ..
FILES=$(find -name '*.java')

for file in $FILES
do
    if cat $file | grep "extends" > /dev/null
    then
        cat $file | perl -0ne 'while (/class (\S+) extends (\S+).*?\{.*}/sg) {print "$2 -> $1 [arrowhead=diamond]\n";}'
    else
        continue
    fi
done

echo "}"

exit $?
