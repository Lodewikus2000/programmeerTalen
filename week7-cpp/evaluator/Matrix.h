#ifndef MATRIX_INCLUDED
#define MATRIX_INCLUDED

#include <vector>
#include <iostream>
#include <string>


#include "evaluator_exception.h"
#include "evaluator_string_tools.h"

/*! Represents a 2 dimensional matrix of doubles.*/
class Matrix
{
    int m_rows,m_cols;
    std::vector<double> m_data;
 public:
    // constructors
    Matrix()                  : m_rows{0},   m_cols{0}                      {}
    Matrix(int rows,int cols) : m_rows{rows},m_cols{cols},m_data(rows*cols) {}

          std::vector<double>& vec()       { return m_data;}
    const std::vector<double>& vec() const { return m_data;}

    int nr_rows() const                         { return m_rows;}
    int nr_cols() const                         { return m_cols;}
          double& operator()(int r,int c)       { return m_data[r*m_cols+c];}
    const double& operator()(int r,int c) const { return m_data[r*m_cols+c];}

    friend std::istream& operator>>(std::istream& is,Matrix& matrix); // give operator access to private variables
};

/*! Reads a Matrix from 'is' stream. */
std::istream& operator>>(std::istream& is,Matrix& matrix)
{

    int rows = 0;
    float fl;


    std::vector<double> data;


    while (is.peek() != EOF) {
        if (is.peek() == '\n'){
            rows ++;
            is.get();
        }
        if (is.peek() == ',') {
            is.get();
        }
        if (is >> fl){
            // std::cout << fl <<"--";
            data.push_back(fl);
        }

    }
    // std::cout << "--"<< rows <<" rows\n";

    int cols = data.size() / rows;

    matrix.m_rows = rows;
    matrix.m_cols = cols;
    matrix.m_data = data;



    return is; // to be completed
}

/*! Writes Matrix 'matrix' to 'os' stream. */
std::ostream& operator<<(std::ostream& os,const Matrix& matrix)
{

    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();


    os << "\n";
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            os << matrix(i, j);
            if (j < cols - 1) {
                os << ',';
            }
        }
        if (i < rows - 1) {
            os << '\n';
        }
    }

    return os; // to be completed
}

/*! Returns a new Matrix that is the negation of 'matrix' */
Matrix operator-(const Matrix& matrix)
{
    return matrix; // to be completed
}

/*! Returns a new Matrix that is the transpose of 'matrix' */
Matrix transpose(const Matrix& matrix)
{
    return matrix; // to be completed
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
Matrix operator+(const Matrix& m1,const Matrix& m2)
{
    return m1; // to be completed
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
Matrix operator-(const Matrix& m1,const Matrix& m2)
{
    return m1; // to be completed
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
Matrix operator*(const Matrix& m1,const Matrix& m2)
{
    return m1; // to be completed
}

#endif
