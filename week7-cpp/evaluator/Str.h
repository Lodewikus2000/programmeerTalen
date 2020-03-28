#include <string>
#include <sstream>
#include <iostream>

class Str
{
public:

    Str()
    {
    }


    std::string get_string() const                     { return m_str;}

    friend std::istream& operator>>(std::istream& is,Str& string); // give operator access to private variables



private:
    std::string m_str;
};


/*! Reads a Str 'string' from 'is' stream. */
std::istream& operator>>(std::istream& is,Str& string)
{

    std::string token;
    is >> token;


    string.m_str = token;

    return is; // to be completed
}

/*! Writes Str 'string' to 'os' stream. */
std::ostream& operator<<(std::ostream& os,const Str& string)
{

    os << string.get_string();

    return os;
}

/*! Returns a new Matrix that is the negation of 'matrix'. */
Str operator-(const Str& string)
{


    return string;
}

/*! Returns a new Matrix that is the transpose of 'matrix'. */
Str transpose(const Str& string)
{

    return string;
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
Str operator+(const Str& string1,const Str& string2)
{


    return string1;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */

Str operator-(const Str& string1,const Str& string2)
{


    return string2;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
Str operator*(const Str& string1,const Str& string2)
{

    return string2;
}
