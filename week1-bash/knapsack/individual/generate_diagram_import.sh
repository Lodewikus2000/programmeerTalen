#!/bin/bash

echo "digraph D {"
cd ..
FILES=$(find -name '*.java')

for file in $FILES
do
    # can seem to match the file names within the perl while statement
    cat $file | perl -0ne 'while(/import (\S+);/sg) {print "-> $1 [arrowhead=dot]\n";}' >> out.txt
done

sort out.txt | uniq
rm out.txt
echo "}"

exit $?
