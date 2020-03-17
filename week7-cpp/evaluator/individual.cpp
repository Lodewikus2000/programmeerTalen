#include <iostream>
using namespace std;

#include "individual.h"

/*! Program that uses the infix_to_postfix() function to read an
    expression from cin, convert it and write it to cout. */
int main()
{
    try
    {
        infix_to_postfix(cin,cout);
    }
    catch(std::exception& e)
    {
        std::cerr<<"exception caught: "<<e.what()<<'\n';
        exit(1);
    }
    return 0;
}
