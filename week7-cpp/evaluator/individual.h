#include <iostream>
#include <stack>

#include "evaluator_exception.h"

/*! Gives the precedence of operator 'ch' and -1
  if it is not an operator.
*/
int precedence(char ch)
{
    switch (ch)
    {
    case '~':case '\'': return 3; // prefix unary minus and prefix transpose operator
    case '*':           return 2; // infix multiplication operator
    case '+':case '-':  return 1; // infix addition and subtraction operators
    case '(':           return 0; // open parenthesis
    }
    return -1;
}

/*! Test if 'ch' is an operator.
*/
bool is_operator(char ch)
{   return precedence(ch)>=0; }

/*! Reads an infix expression and returns the equivalent postfix
    expression where each element is separated by a whitespace
    character. For example reading "aa+bb*cc" results in writting
    "aa bb cc * + ".
*/
void infix_to_postfix(istream& is,ostream& os)
{
    char ch;
    while (is>>ch)
    {
        os<<ch; // to be completed
    }
    os<<'\n';
}
