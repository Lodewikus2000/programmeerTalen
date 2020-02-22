#!/bin/bash

E_BADARGS=65

if [ $# -ne 2 ]
then
    echo "Usage: `basename $0` [old-name] [new-name]"
    exit $E_BADARGS
fi

#argumenten opschonen

FILE=$(find .. -name $1'.java')

NEW_FILENAME=$(echo $FILE | perl -pe "s/$1/$2/s")
cat $FILE | perl -pe "s/class\s$1/class $2/sg" > $NEW_FILENAME
rm $FILE

exit $?
