/* Pim van Helvoirt
 * Leo Schreuders
 */

#ifndef MATRIXT_INCLUDED
#define MATRIXT_INCLUDED

#include <vector>
#include <iostream>
#include <algorithm>
#include <sstream>

#include "Str.h"

#include "evaluator_exception.h"
#include "evaluator_string_tools.h"

/*! Represents a 2 dimensional matrix of template type T. */
template<typename T>
class MatrixT
{
    int m_rows,m_cols;
    std::vector<T> m_data;
 public:
    // constructors
    MatrixT()                  : m_rows{0},   m_cols{0}                      {}
    MatrixT(int rows,int cols) : m_rows{rows},m_cols{cols},m_data(rows*cols) {}

          std::vector<T>& vec()       { return m_data;}
    const std::vector<T>& vec() const { return m_data;}

    int nr_rows() const                     { return m_rows;}
    int nr_cols() const                     { return m_cols;}
          T& operator()(int r,int c)        { return m_data[r*m_cols+c];}
    const T& operator()(int r,int c) const  { return m_data[r*m_cols+c];}

    // give operator access to private variables
    template<typename T2> friend std::istream& operator>>(std::istream& is,
                                                          MatrixT<T2>& matrix);
};

/*! Reads a Matrix from 'is' stream. */
template<typename T>
std::istream& operator>>(std::istream& is,MatrixT<T>& matrix)
{
    int rows = 0;


    std::vector<T> data;

    std::vector<std::string> tokens;
    std::string line;

    std::string token;

    // Tokenize the input stream
    while (std::getline(is, line))
    {
        rows++;

        std::stringstream ss(line);
        while (std::getline(ss, token, ',')) {
            trim(token);
            tokens.push_back(token);
        }

    }

    for (size_t i = 0; i < tokens.size(); i++) {
        std::istringstream ss(tokens[i]);
        T element;
        ss >> element;
        data.push_back(element);
    }

    // std::cout << "--"<< rows <<" rows\n";

    int cols = data.size() / rows;

    matrix.m_rows = rows;
    matrix.m_cols = cols;
    matrix.m_data = data;

    return is; // to be completed
}

/*! Writes Matrix 'matrix' to 'os' stream. */
template<typename T>
std::ostream& operator<<(std::ostream& os,const MatrixT<T>& matrix)
{

    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            os << matrix(i, j);
            if (j < cols - 1) {
                os << ',';
            }
        }
        if (i < rows - 1) {
            os << "\n";
        }
    }

    return os;
}

/*! Returns a new Matrix that is the negation of 'matrix'. */
template<typename T>
MatrixT<T> operator-(const MatrixT<T>& matrix)
{
    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();

    std::vector<T> new_data;

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            new_data.push_back(-matrix(i, j));
        }
    }

    MatrixT<T> new_matrix(rows, cols);
    new_matrix.vec() = new_data;

    // MatrixT<T> new_matrix(rows, cols);
    // new_matrix.vec() = matrix.vec();
    //
    // std::vector<T> data = new_matrix.vec();
    //
    // auto negate = [](double n){n *= -1.0;};
    //
    // std::for_each(data.begin(), data.end(), negate);

    return new_matrix;
}

/*! Returns a new Matrix that is the transpose of 'matrix'. */
template <typename T>
MatrixT<T> transpose(const MatrixT<T>& matrix)
{
    int rows = matrix.nr_rows();
    int cols = matrix.nr_cols();
    // std::cout << "rows: " << rows << "\n";
    // std::cout << "cols: " << cols << "\n";


    std::vector<T> new_data;


    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            new_data.push_back(matrix(i, j));
        }
    }
    MatrixT<T> new_matrix(cols, rows);
    new_matrix.vec() = new_data;

    // for (int j = 0; j < new_data.size(); j++) {
    //     std::cout <<"\n" << new_data[j] << "\n";
    // }
    //
    // std::cout << "rows: " << new_matrix.nr_rows() << "\n";
    // std::cout << "cols: " << new_matrix.nr_cols() << "\n";

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1+m2'. */
template<typename T>
MatrixT<T> operator+(const MatrixT<T>& m1,const MatrixT<T>& m2)
{
    int rows = m1.nr_rows();
    int cols = m1.nr_cols();

    std::vector<T> new_data;


    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            new_data.push_back(m1(i, j) + m2(i, j));
        }
    }
    MatrixT<T> new_matrix(rows, cols);
    new_matrix.vec() = new_data;

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1-m2'. */
template<typename T>
MatrixT<T> operator-(const MatrixT<T>& m1,const MatrixT<T>& m2)
{
    int rows = m1.nr_rows();
    int cols = m1.nr_cols();

    std::vector<T> new_data;


    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            new_data.push_back(m1(i, j) - m2(i, j));
        }
    }
    MatrixT<T> new_matrix(rows, cols);
    new_matrix.vec() = new_data;

    return new_matrix;
}

/*! Returns a new Matrix that is equal to 'm1*m2'. */
template<typename T>
MatrixT<T> operator*(const MatrixT<T>& m1,const MatrixT<T>& m2)
{
    int m = m1.nr_rows();
    int n = m1.nr_cols();
    int n2 = m2.nr_rows();
    int p = m2.nr_cols();

    if (n != n2) {
        throw Evaluator_exception("Invalid dimensions"
                                  " in matrix multiplication");
    }


    std::vector<T> new_data;

    T sum{};
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < p; j++) {
            sum = T();
            for (int r = 0; r < n; r++) {
                sum = sum + m1(i, r) * m2(r, j);
            }
            new_data.push_back(sum);
        }
    }
    MatrixT<T> new_matrix(m, p);
    new_matrix.vec() = new_data;

    return new_matrix;
}

#endif
