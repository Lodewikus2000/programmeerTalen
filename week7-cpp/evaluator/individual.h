// Leo Schreuders
// Programmeertalen


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
    }
    return -1;
}

/*! Test if 'ch' is an operator.
*/
bool is_operator(char ch)
{   return precedence(ch)>=0; }



/*! Return associativity. 0 is left, 1 is right.
*/
int associativity(char ch)
{
    switch (ch)
    {
    case '+':case '-': case '*':    return 0;
    case '~':case '\'':                             return 1;
    }
    return -1;
}



/*! Reads an infix expression and returns the equivalent postfix
    expression where each element is separated by a whitespace
    character. For example reading "aa+bb*cc" results in writting
    "aa bb cc * + ".
*/
void infix_to_postfix(istream& is,ostream& os)
{
    char ch;
    std::stack<char> op_stack;


    bool saw_op = false;


    while (is>>ch)
    {
        if (isalnum(ch)) {

            if (saw_op) {
                os<<" ";
                saw_op = false;
            }

            os << ch;

        } else if (is_operator(ch)) {

            saw_op = true;


            while ( (!op_stack.empty()) &&
                        (   ( precedence(op_stack.top()) > precedence(ch) )
                            ||
                            (   (precedence(op_stack.top()) == precedence(ch))
                                && associativity(ch) == 0 )
                    )
                    && ( op_stack.top() != '(')
                  ) {

                char top = op_stack.top();

                os<<" ";
                os<<top;

                op_stack.pop();
            }

            op_stack.push(ch);

        } else if (ch == '(') {

            op_stack.push(ch);

        } else if (ch == ')') {

            while (!op_stack.empty() && op_stack.top() != '(') {
                char top = op_stack.top();

                os<<" ";
                os<<top;

                op_stack.pop();

            }
            if (!op_stack.empty() && op_stack.top() == '(') {
                op_stack.pop();
            }
        } else {
                if (saw_op) {
                    os<<" ";
                    saw_op = false;
                }
                os<<ch;
        }

    }
    while (!op_stack.empty()) {
        char top = op_stack.top();
        os<<" ";
        os<<top;

        op_stack.pop();

    }
    os<<'\n';
}
