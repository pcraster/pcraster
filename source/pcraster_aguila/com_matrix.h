#ifndef INCLUDED_COM_MATRIX
#define INCLUDED_COM_MATRIX



#include <algorithm>
#include <cassert>
#include <cmath>
#include <stdexcept>

namespace com {
  template<class T>
    class Matrix;

  template<class T>
    bool           operator==          (const Matrix<T> &lhs,
                                        const Matrix<T> &rhs);
  template<class T>
    bool           operator!=          (const Matrix<T> &lhs,
                                        const Matrix<T> &rhs);
  template<class T>
    Matrix<T>      operator*           (const Matrix<T> &lhs,
                                        const Matrix<T> &rhs);

  template<class T>
    Matrix<T>      operator+           (const Matrix<T> &lhs,
                                        const Matrix<T> &rhs);
  template<class T>
    Matrix<T>      operator-           (const Matrix<T> &lhs,
                                        const Matrix<T> &rhs);
}

namespace com {



/*!
  \class Matrix
  \brief The Matrix class is for matrix objects(!).

  It is a very straightforward implementation of some matrix manipulations.

  Sources: http://cgl.bu.edu/GC/shammi/p5/

  Todo: invert should use lu decomposition and do something smart with singular
  matrices.
*/
//       1         2         3         4         5         6         7         8
template<class T>
class Matrix
{
private:

  //! Number of rows.
  size_t           d_nr;

  //! Number of cols.
  size_t           d_nc;

  //! Values in the matrix.
  T *              d_data;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   Matrix              (size_t nr);

  //! Constructor.
                   Matrix              (size_t nr,
                                        size_t nc);

  //! Constructor.
                   Matrix              (size_t nr,
                                        size_t nc,
                                        const T &v);

  //! Constructor.
                   Matrix              (size_t nr,
                                        size_t nc,
                                        T* v);

  //! Copy constructor.
                   Matrix              (const Matrix &m);

  //! Destructor.
  /* virtual */    ~Matrix             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Matrix &         operator=           (const Matrix &m);

  //! Sets the element at position \a r, \a c to \a v.
  void             setElement          (size_t r,
                                        size_t c,
                                        const T &v);

  //! Sets all elements to \a v.
  void             setElements         (const T &v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the number of rows.
  size_t           nrRows              () const;

  //! Returns the number of columns.
  size_t           nrCols              () const;

  //! Returns the element at position \a r, \a c.
  const T &        element             (size_t r,
                                        size_t c) const;

  //! Returns all elements as an array.
  const T *        data                () const;

  //! Equality operator.
  friend bool      operator==<>        (const Matrix &lhs,
                                        const Matrix &rhs);

  //! Inequality operator.
  friend bool      operator!=<>        (const Matrix &lhs,
                                        const Matrix &rhs);

  //! Multiplies \a lhs with \a rhs and returns the result.
  friend Matrix<T> operator*<>         (const Matrix &lhs,
                                        const Matrix &rhs);

  //! Adds \a rhs to \a lhs and returns the result.
  friend Matrix<T> operator+<>         (const Matrix &lhs,
                                        const Matrix &rhs);

  //! Substracts \a rhs from \a lhs and returns the result.
  friend Matrix<T> operator-<>         (const Matrix &lhs,
                                        const Matrix &rhs);

  //----------------------------------------------------------------------------
  // STATIC FUNCTIONS
  //----------------------------------------------------------------------------

  //! Creates an identity matrix with dimensions \a nr x \a nr.
  static Matrix    identity            (size_t nr);

  //! Returns an inverted version of \a m.
  static Matrix    invert              (const Matrix &m);

  //! Returns the transposed version of \a m.
  static Matrix    transpose           (const Matrix &m);

  //! LU decompose matrix \a a.
  static void      luDecompose         (Matrix &a,
                                        size_t *indx,
                                        double *d);

  //! LU solves the set of linear equations.
  static void      luSolve             (const Matrix &a,
                                        const size_t *indx,
                                        double *b);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

/*!
  \param     nr Dimension of the matrix.
  \sa        Matrix(size_t, size_t), Matrix(size_t, size_t, const T &)

  Creates a square matrix with dimensions \a nr x \a nr and initializes all
  elements to 0.
*/
template<class T>
inline Matrix<T>::Matrix(size_t nr)
  : d_nr(nr), d_nc(nr)
{
  assert(d_nr == d_nc);
  assert(d_nr > 0);

  d_data = new T[d_nr * d_nc];
  setElements(static_cast<T>(0));
}

/*!
  \param     nr Number of rows.
  \param     nc Number of columns.
  \sa        Matrix(size_t), Matrix(size_t, size_t, const T &)

  Initializes all elements to 0.
*/
template<class T>
inline Matrix<T>::Matrix(size_t nr, size_t nc)
  : d_nr(nr), d_nc(nc)
{
  assert(d_nr > 0 && d_nc > 0);

  d_data = new T[d_nr * d_nc];
  setElements(static_cast<T>(0));
}

/*!
  \param     nr Number of rows.
  \param     nc Number of columns.
  \param     v Value.
  \sa        Matrix(size_t), Matrix(size_t, size_t)

  Initializes all elements to \a v.
*/
template<class T>
inline Matrix<T>::Matrix(size_t nr, size_t nc, const T &v)
  : d_nr(nr), d_nc(nc)
{
  assert(d_nr > 0 && d_nc > 0);

  d_data = new T[d_nr * d_nc];
  setElements(v);
}

template<class T>
inline Matrix<T>::Matrix(size_t nr, size_t nc, T* v)
  : d_nr(nr), d_nc(nc)
{
  assert(d_nr > 0 && d_nc > 0);

  d_data = new T[d_nr * d_nc];
  std::copy(v, v + d_nr * d_nc, d_data);
}

/*!
  \param     m Matrix from which the properties and values should be copied.
*/
template<class T>
inline Matrix<T>::Matrix(const Matrix &m)
  : d_nr(m.d_nr), d_nc(m.d_nc)
{
  d_data = new T[d_nr * d_nc];
  for(size_t i = 0; i < d_nr * d_nc; ++i)
    d_data[i] = m.d_data[i];
}

template<class T>
inline Matrix<T>::~Matrix()
{
  delete [] d_data;
}

/*!
  \param     m Matrix from which the values should be copied.
  \return    Assigned matrix.
  \warning   The dimensions of both matrices must be de same.
*/
template<class T>
inline Matrix<T> &Matrix<T>::operator=(const Matrix &m)
{
  assert(d_nr == m.d_nr);
  assert(d_nc == m.d_nc);

  if(this != &m) {
    for(size_t i = 0; i < d_nr * d_nc; ++i) {
      d_data[i] = m.d_data[i];
    }
  }

  return *this;
}

/*!
  \return    Number of rows.
  \sa        nrCols()
*/
template<class T>
size_t Matrix<T>::nrRows() const
{
  return d_nr;
}

/*!
  \return    Number of columns.
  \sa        nrRows()
*/
template<class T>
size_t Matrix<T>::nrCols() const
{
  return d_nc;
}

/*!
  \param     r Row number.
  \param     c Column number.
  \return    Value of element at position \a r, \a c.
  \warning   Row and column numbers start with 1.
  \sa        data(), setElement(size_t, size_t, const T &v),
             setElements(const T &v)
*/
template<class T>
inline const T &Matrix<T>::element(size_t r, size_t c) const
{
  assert(r > 0 && c > 0);
  assert(r <= d_nr && c <= d_nc);

  return d_data[(r - 1) * d_nc + c - 1];
}

/*!
  \return    Array with all elements.
  \warning   If you want to use the contents of a matrix as a transformation
             matrix in OpenGL, you first have to call transpose() on the matrix.
             OpenGL orders its matrices different.
  \sa        element(size_t, size_t)

  The first element in the array corresponds with the matrix element at
  position [1, 1]. The second element corresponds with matrix element [1, 2].

  data = [ [r1, c1], [r1, c2], ... [ri, cj-1], [ri, cj] ]
*/
template<class T>
inline const T *Matrix<T>::data() const
{
  return d_data;
}

/*!
  \param     r Row number.
  \param     c Column number.
  \param     v Value.
  \warning   Row and column numbers start with 1.
  \sa        setElements(const T &v), element(size_t, size_t)
*/
template<class T>
inline void Matrix<T>::setElement(size_t r, size_t c, const T &v)
{
  assert(r > 0 && c > 0);
  assert(r <= d_nr && c <= d_nc);

  d_data[(r - 1) * d_nc + c - 1] = v;
}

/*!
  \param     v Value.
  \sa        setElement(size_t, size_t, const T &v), element(size_t, size_t)
*/
template<class T>
inline void Matrix<T>::setElements(const T &v)
{
  for(size_t i = 0; i < d_nr * d_nc; ++i)
    d_data[i] = v;
}

//------------------------------------------------------------------------------
// STATIC FUNCTIONS
//------------------------------------------------------------------------------

/*!
  \param     nr Dimension of the matrix.
  \return    Identity matrix.
*/
template<class T>
inline Matrix<T> Matrix<T>::identity(size_t nr)
{
  assert(nr > 0);

  // Create a matrix with all zero's.
  Matrix<T> m(nr);

  // Set the diagonal elements to 1.
  for(size_t i = 0; i < m.d_nr; ++i)
    m.d_data[i * m.d_nc + i] = static_cast<T>(1);

  return m;
}

/*!
  \param     m Matrix to transpose.
  \return    The transposed matrix.
*/
template<class T>
inline Matrix<T> Matrix<T>::transpose(const Matrix<T> &m)
{
  // Create a matrix with nr and nc swapped.
  Matrix<T> t(m.d_nc, m.d_nr);

  // Copy values from m to the new matrix.
  for(size_t r = 1; r <= m.d_nr; ++r)
    for(size_t c = 1; c <= m.d_nc; ++c)
      t.setElement(c, r, m.element(r, c));

  return t;
}

/*!
  \param     m Matrix to invert.
  \return    Inverted matrix.
  \warning   .
*/
template<class T>
inline Matrix<T> Matrix<T>::invert(const Matrix<T> &m)
{
  assert(m.d_nr == m.d_nc);

  size_t i, j;
  double d;

  // Create a scratch copy of m.
  Matrix<T> scratch(m);

  // Create the result matrix with same dimensions as m.
  Matrix<T> inverse(m.d_nr);

  size_t *indx = new size_t[m.d_nr];
  T *col = new T[m.d_nr];

  try
  {
    // Decompose the matrix.
    luDecompose(scratch, indx, &d);

    // Loop over the columns.
    for(j = 1; j <= scratch.d_nr; ++j)
    {
      // Set the scratch column to 0's
      for(i = 1; i <= scratch.d_nr; ++i)
        col[i - 1] = static_cast<T>(0);
      col[j - 1] = static_cast<T>(1);

      // Solve for the scratch column.
      luSolve(scratch, indx, col);

      // Copy scratch column's values to the result matrix.
      for(i = 1; i <= scratch.d_nr; ++i)
        inverse.setElement(i, j, col[i - 1]);
    }

    delete[] indx;
    delete[] col;
  }
  catch(...)
  {
    delete[] indx;
    delete[] col;
    throw;
  }

  return inverse;



/*
  assert(m.d_nr == m.d_nc);

  // lu decomposition could be used here

  Matrix<T> scratch(m);
  Matrix<T> result = identity(scratch.d_nr);

  size_t i, j, k;
  T tmp1, tmp2;
  for(j = 0; j < scratch.d_nc; j++)
  {
    for(i = 0; i < scratch.d_nr; i++)
    {
      if(i != j)
      {
        tmp1 = scratch.d_data[j * scratch.d_nc + j];
        tmp2 = scratch.d_data[i * scratch.d_nc + j];

        for(k = 0; k < scratch.d_nc; k++)
        {
          scratch.d_data[i * scratch.d_nc + k] =
                   scratch.d_data[i * scratch.d_nc + k] * tmp1 -
                   scratch.d_data[j * scratch.d_nc + k] * tmp2;
          result.d_data[i * scratch.d_nc + k] =
                   result.d_data[i * scratch.d_nc + k] * tmp1 -
                   result.d_data[j * scratch.d_nc + k] * tmp2;
        }
      }
    }
  }

  for(i = 0; i < scratch.d_nr; ++i)
  {
    tmp1 = scratch.d_data[i * scratch.d_nc + i];

    assert(tmp1 != static_cast<T>(0));


    // Check if the matrix is non-invertible.
//    if(tmp1 == static_cast<T>(0))
//    {
//      result.setElements(static_cast<T>(0));
//      break;
//    }

    for(k = 0; k < result.d_nc; k++)
      result.d_data[i * scratch.d_nc + k] /= tmp1;
  }

  return result;
*/
}

// #define TINY 1.0e-20         // A small number.

template<typename T>
inline T abs(T value);

template<>
inline float abs(float value)
{
  return fabsf(value);
}

template<>
inline double abs(double value)
{
  return fabs(value);
}

template<class T>
inline void Matrix<T>::luDecompose(Matrix<T> &a, size_t *indx, double *d)
{
  assert(a.d_nr == a.d_nc);

  size_t n = a.d_nr;
  size_t i, j, k;
  size_t imax = 0;
  double *vv = new double[n];
  T big, dum, sum, temp;

  *d = 1.0;

  try
  {
    for(i = 1; i <= n; i++)
    {
      big = static_cast<T>(0);
      for(j = 1; j <= n; j++)
      {
        if((temp = abs<T>(a.element(i, j))) > big) {
          big = temp;
        }
      }

      if(big == static_cast<T>(0))
        throw std::logic_error("singular matrix");

      vv[i - 1] = 1.0 / big;
    }

    for(j = 1; j <= n; j++)
    {
      for(i = 1; i < j; i++)
      {
        sum = a.element(i, j);
        for(k = 1; k < i; k++)
          sum -= a.element(i, k) * a.element(k, j);
        a.setElement(i, j, sum);
      }

      big = static_cast<T>(0);
      for(i = j; i <= n; i++)
      {
        sum = a.element(i, j);
        for(k = 1; k < j; k++)
          sum -= a.element(i, k) * a.element(k, j);
        a.setElement(i, j, sum);
        if((dum = vv[i - 1] * std::abs(sum)) >= big)
        {
          big = dum;
          imax = i;
        }
      }

      if(j != imax)
      {
        for(k = 1; k <= n; k++)
        {
          dum = a.element(imax, k);
          a.setElement(imax, k, a.element(j, k));
          a.setElement(j, k, dum);
        }

        *d = -(*d);
        vv[imax - 1] = vv[j - 1];
      }

      indx[j - 1] = imax;
      if(a.element(j, j) == static_cast<T>(0))
        throw std::logic_error("singular matrix");
        // a.setElement(j, j, TINY);

      if(j != n)
      {
        dum = 1.0 / a.element(j, j);
        for(i = j + 1; i <= n; i++)
          a.setElement(i, j, a.element(i, j) * dum);
      }
    }

    delete[] vv;
  }
  catch(...)
  {
    delete[] vv;
    throw;
  }
}

template<class T>
inline void Matrix<T>::luSolve(const Matrix<T> &a, const size_t *indx,
                   double *b)
{
  assert(a.d_nr == a.d_nc);

  size_t n = a.d_nr;
  size_t i, ii = 0, ip, j;
  T sum;

  for(i = 1; i <= n; i++)
  {
    ip = indx[i - 1];
    sum = b[ip - 1];
    b[ip - 1] = b[i - 1];
    if(ii)
      for(j = ii; j <= i - 1; j++)
        sum -= a.element(i, j) * b[j - 1];
    else if(sum)
      ii = i;
    b[i - 1] = sum;
  }

  for(i = n; i >= 1; i--)
  {
    sum = b[i - 1];
    for(j = i + 1; j <= n; j++)
      sum -= a.element(i, j) * b[j - 1];
    b[i - 1] = sum / a.element(i, i);
  }
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<class T>
bool operator==(const Matrix<T> &lhs, const Matrix<T> &rhs)
{
  assert(lhs.d_nr == rhs.d_nr);
  assert(lhs.d_nc == rhs.d_nc);

  for(size_t i = 0; i < lhs.d_nr * lhs.d_nc; ++i)
  {
    if(lhs.d_data[i] != rhs.d_data[i])
      return false;
  }

  return true;
}

template<class T>
bool operator!=(const Matrix<T> &lhs, const Matrix<T> &rhs)
{
  assert(lhs.d_nr == rhs.d_nr);
  assert(lhs.d_nc == rhs.d_nc);

  for(size_t i = 0; i < lhs.d_nr * lhs.d_nc; ++i)
  {
    if(lhs.d_data[i] != rhs.d_data[i])
      return true;
  }

  return false;
}

/*!
  \param     lhs Matrix to multiply.
  \param     rhs Matrix to multiply with.
  \return    Result matrix.
  \warning   The number of columns of \a lhs must equal the number of rows of
             \a rhs.
*/
template<class T>
Matrix<T> operator*(const Matrix<T> &lhs, const Matrix<T> &rhs)
{
  assert(lhs.d_nc == rhs.d_nr);

  Matrix<T> m(lhs.d_nr, rhs.d_nc);
  for(size_t i = 0; i < lhs.d_nr; ++i) {
    for(size_t k = 0; k < rhs.d_nc; ++k) {
      for(size_t j = 0; j < lhs.d_nc; ++j) {
        m.d_data[i * m.d_nc + k] += lhs.d_data[i * lhs.d_nc + j] *
                   rhs.d_data[j * rhs.d_nc + k];
      }
    }
  }

  return m;
}



/*!
  \param     lhs Matrix to add to.
  \param     rhs Matrix to add.
  \return    Result matrix.
  \warning   The dimensions of both matrices must be equal.
*/
template<class T>
Matrix<T> operator+(const Matrix<T> &lhs, const Matrix<T> &rhs)
{
  assert(lhs.d_nr == rhs.d_nr);
  assert(lhs.d_nc == rhs.d_nc);

  Matrix<T> m(lhs.d_nr, lhs.d_nc);
  for(size_t r = 0; r < lhs.d_nr; ++r)
    for(size_t c = 0; c < lhs.d_nc; ++c)
      m.d_data[r * lhs.d_nc + c] = lhs.d_data[r * lhs.d_nc + c] +
                   rhs.d_data[r * lhs.d_nc + c];

  return m;
}

/*!
  \param     lhs Matrix to substract from.
  \param     rhs Matrix to substract.
  \return    Result matrix.
  \warning   The dimensions of both matrices must be equal.
*/
template<class T>
Matrix<T> operator-(const Matrix<T> &lhs, const Matrix<T> &rhs)
{
  assert(lhs.d_nr == rhs.d_nr);
  assert(lhs.d_nc == rhs.d_nc);

  Matrix<T> m(lhs.d_nr, lhs.d_nc);
  for(size_t r = 0; r < lhs.d_nr; ++r)
    for(size_t c = 0; c < lhs.d_nc; ++c)
      m.d_data[r * lhs.d_nc + c] = lhs.d_data[r * lhs.d_nc + c] -
                   rhs.d_data[r * lhs.d_nc + c];

  return m;
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#ifdef DEBUG_DEVELOP

/*!
  \relates   com::Matrix
  \brief     Writes the contents of the matrix to stream \a s.
  \param     s Stream to write to.
  \param     m Matrix to write.
  \return    Output stream.
*/
template<class T>
std::ostream &operator<<(std::ostream &s, const com::Matrix<T> &m)
{
  for(size_t r = 0; r < m.nrRows(); ++r)
  {
    for(size_t c = 0; c < m.nrCols(); ++c)
      s << m.element(r + 1, c + 1) << '\t';
    s << '\n';
  }
  return s;
}

#endif

#endif
