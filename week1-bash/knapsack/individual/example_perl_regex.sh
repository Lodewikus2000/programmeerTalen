#!/bin/bash

multi_line_string="1 2 3 Axx\nxxxB 4 5 \n6 AxxxB 7 8 AxxxxxxxxxxxB"
echo -e "multi_line_string: '$multi_line_string'"

echo -e "\n=== substitute FIRST A...B string with '###':"
echo -e "$multi_line_string" | perl -0pe 's/A.*?B/###/s'

echo -e "\n=== substitute ALL A...B strings with '###':"
echo -e "$multi_line_string" | perl -0pe 's/A.*?B/###/sg'

#  perl -0pe 's/A.*?B/###/sg'
#        |||  |           ||
#        |||  |           |+- global matching, continue where last match ended
#        |||  |           +- '.' also matches new-line character
#        |||  +- 's/X/Y/' substitutes X with Y
#        ||+- '<perl-program>' given as argument
#        |+- run in a loop and print input/result string
#        +- read multi-line string as single input

echo -e "\n=== get FIRST A...B substring:"
echo -e "$multi_line_string" | perl -0ne 'if (/(A.*?B)/s) {print "- $1\n";}'

echo -e "\n=== get ALL A...B substrings:"
echo -e "$multi_line_string" | perl -0ne 'while (/(A.*?B)/sg) {print "- $1\n";}'

#  perl -0ne 'while (/(A.*?B)/sg) {print "- $1\n";}'
#         |
#         +- run in a loop but do NOT print input/result string

balanced_brackets_string="{....{....}..}...}.{.{.{.}"
echo -e "\nbalanced_brackets_string: '$balanced_brackets_string'"

echo -e "\n=== substitute between balanced open and closing brackets using a regex subroutine"
echo $balanced_brackets_string | perl -0pe 's/({([^{}]|(?1))*})/###/sg'

#  perl -0pe 's/({([^{}]|(?1))*})/###/sg'
#               || |     |     ||
#               || |     |     |+- end of group1
#               || |     |     +- closing bracket
#               || |     +- subroutine to group1 (recursive as it is in group1 itself)
#               || +- everything but the { } characters
#               |+- opening bracket
#               +- start of group1                           
