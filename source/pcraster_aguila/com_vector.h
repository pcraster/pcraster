#ifndef INCLUDED_COM_VECTOR
#define INCLUDED_COM_VECTOR



#include <algorithm>
#include <cassert>
#include <cmath>



namespace com {

  template<class T>
    class Vector;

  template<class T>
    Vector<T> operator+          (const Vector<T>& lhs,
                                  const Vector<T>& rhs);


/*!
  \class Vector
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
template<class T>
class Vector
{

private:

  //! Size of vector.
  size_t           d_size;

  //! Values in the vector.
  T *              d_data;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Vector              (size_t s);

                   Vector              (const Vector& v);

  /* virtual */    ~Vector             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Vector&          operator=           (const Vector& v);

  void             setElement          (size_t p,
                                        const T& v);

  void             add                 (const Vector& v);

  void             scale               (double s);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           size                () const;

  const T&         element             (size_t p) const;

  double           magnitude           () const;

  friend Vector<T> operator+<>         (const Vector<T>& lhs,
                                        const Vector<T>& rhs);

  //----------------------------------------------------------------------------
  // STATIC FUNCTIONS
  //----------------------------------------------------------------------------

  static double    dot                 (const Vector& v1,
                                        const Vector& v2);

  static Vector    cross               (const Vector& v1,
                                        const Vector& v2);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

template<class T>
inline Vector<T>::Vector(size_t s)
  : d_size(s)
{
  d_data = new T[d_size];
  std::fill(d_data, d_data + d_size, static_cast<T>(0));
}



template<class T>
inline Vector<T>::Vector(const Vector& v)
  : d_size(v.d_size)
{
  d_data = new T[d_size];
  std::copy(v.d_data, v.d_data + v.d_size, d_data);
}



template<class T>
inline Vector<T>::~Vector()
{
  delete [] d_data;
}



template<class T>
inline Vector<T>& Vector<T>::operator=(const Vector& v)
{
  assert(d_size == v.d_size);

  if(this != &v)
    std::copy(v.d_data, v.d_data + v.d_size, d_data);
  return *this;
}



template<class T>
void Vector<T>::setElement(size_t p, const T& v)
{
  assert(p > 0 && p <= d_size);

  d_data[p - 1] = v;
}



template<class T>
void Vector<T>::add(const Vector& v)
{
  assert(d_size == v.d_size);

  for(size_t i = 0; i < d_size; ++i)
    d_data[i] += v.d_data[i];
}



template<class T>
void Vector<T>::scale(double s)
{
  for(size_t i = 0; i < d_size; ++i)
    d_data[i] *= s;
}

template<class T>
size_t Vector<T>::size() const
{
  return d_size;
}

template<class T>
const T& Vector<T>::element(size_t p) const
{
  assert(p > 0 && p <= d_size);

  return d_data[p - 1];
}

template<class T>
double Vector<T>::magnitude() const
{
  double m = 0;

  for(size_t i = 0; i < d_size; ++i)
    m += static_cast<double>(d_data[i] * d_data[i]);
  return std::sqrt(m);
}

//------------------------------------------------------------------------------
// STATIC FUNCTIONS
//------------------------------------------------------------------------------

// dot product, scalar product, inner product
template<class T>
inline double Vector<T>::dot(const Vector& v1, const Vector& v2)
{
  assert(v1.d_size == v2.d_size);

  double p = 0;
  for(size_t i = 0; i < v1.d_size; ++i)
    p += v1.d_data[i] * v2.d_data[i];
  return p;
}



// cross product, vector product
template<class T>
inline Vector<T> Vector<T>::cross(const Vector& v1, const Vector& v2)
{
  assert(v1.d_size == 3);
  assert(v2.d_size == 3);

  Vector<T> v(3);
  v.d_data[0] = v1.d_data[1] * v2.d_data[2] - v1.d_data[2] * v2.d_data[1];
  v.d_data[1] = v1.d_data[2] * v2.d_data[0] - v1.d_data[0] * v2.d_data[2];
  v.d_data[2] = v1.d_data[0] * v2.d_data[1] - v1.d_data[1] * v2.d_data[0];
  return v;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<class T>
Vector<T> operator+(const Vector<T>& lhs, const Vector<T>& rhs)
{
  assert(lhs.d_size == rhs.d_size);

  Vector<T> v(lhs);
  v.add(rhs);
  return v;
}


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
