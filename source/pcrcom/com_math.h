#ifndef INCLUDED_COM_MATH
#define INCLUDED_COM_MATH

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_CSTDLIB
#include <cstdlib>
#define INCLUDED_CSTDLIB
#endif
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif
// PCRaster library headers.

// Module headers.



namespace com {
  // Math declarations.
}

/*!
  \file
  This file includes cmath and cstdlib and a number of convenience funtions
*/


namespace com {

//! value to the power 2
template<class T>
T pow2(T value) {
  return value*value;
}

//! whole number std::ceil
template<class Int,class Value>
Int ceil(Value value) {
  return static_cast<Int>(std::ceil(value));
}

//! whole number std::floor
template<class Int,class Value>
Int floor(Value value) {
  return static_cast<Int>(std::floor(value));
}

//! return difference of 2 numbers as a fraction
/*!
 * fraction is a percentage, for example a value 0.32 means
 * that \a a and \a b differ about 32 %
 * Value 1 and 2 differ by 50%, that is 50 % of the largest of
 * the values \a a and \a b.
 */
template<class Real>
inline Real fractionDifference(Real a, Real b)
{
  if (a == 0.0 || b == 0.0) {
    // one of them really 0 makes fraction BIG
    a+=1;
    b+=1;
  }
   // found this code on the NET
  Real d=std::max(static_cast<Real>(1),
         static_cast<Real>(std::max(std::fabs(a),std::fabs(b))));
  return std::fabs(a-b)/d;
}

#define COM_DEFAULT_EPSILON  0.000001

//! compare 2 numbers to be in a certain epsilon
/*!
 * \param a value
 * \param b value
 * \param eps the fractionDifference accepted, e..g 0.01 = 1%
 */
template<class Real>
inline bool equal_epsilon(Real a, Real b, Real eps=COM_DEFAULT_EPSILON)
{
   // found this code on the NET
  return fractionDifference(a,b) < eps;
}

//! Maximize a value with its own value and another value
/* 
 * \param   x value to maximize, x = max(x,o)
 * \param   o other value
 */
template<class T>
inline void maximize(T &x, const T &o)
{
  x=std::max<T>(x,o);
}

//! Minimize a value with its own value and another value
/* 
 * \param   x value to minimize, x = min(x,o)
 * \param   o other value
 */
template<class T>
inline void minimize(T &x, const T &o)
{
  x=std::min<T>(x,o);
}

//!  Limit a value to fit into a specified interval.
/* 
 * \param   x Input value.
 * \param   low low boundary.
 * \param   high high boundary.
 * \return  Limited version of \a x.
 *
 * If \a x is smaller than low then low is returned.
 * If \a x is higher than high than high is returned
 * else \a x is returned.
 */
template<class ValueType>
inline ValueType   lim                 (const ValueType &x,
                                        const ValueType &low,
                                        const ValueType &high)
{
  DEVELOP_PRECOND(low <= high);

  if(x < low)
    return low;
  if(x > high)
    return high;
  return x;
}

//!  Like lim() but boundary argument may be swapped
template<class ValueType>
inline ValueType   limUnordered        (const ValueType &x,
                                        const ValueType &x1,
                                        const ValueType &x2)
{
  if (x1 > x2)
    return lim(x,x2,x1);
  return lim(x,x1,x2);
}

//! absolute function for arbitrary types.
/*!
 * Seems needed since std namespace has fabs (real numbers) and abs (integral).
 * Note that fabs is faster if one needs it only for real numbers.
 */
template<class ValueType>
inline ValueType   absolute(const ValueType &value)
{
  return value > 0 ? value : -value;
}

template<class T>
inline int         sign                (const T &x)
{
  if(x > static_cast<T>(0))
    return 1;
  else if(x < static_cast<T>(0))
    return -1;
  else
    return 0;
}

//! is value an integer, e.g. does it  not have a fractional part
inline bool isInteger(const double& v)
{
  return std::fmod(v,1) == 0.0;
}

double interpolate2(
    double x,
    double x1,
    double y1,
    double x2,
    double y2);

template<typename T>
T megaByte(T nr=1) {
  return 1024*1024*nr;
}

template<typename T>
T gigaByte(T nr=1) {
  return 1024*megaByte<T>()*nr;
}

template<typename T>
 struct NumericLimits : public std::numeric_limits<T> {
  //! smallest reprentable value
  static T minValue() throw() {
      BOOST_STATIC_ASSERT(std::numeric_limits<T>::is_integer);
      return std::numeric_limits<T>::min();
  }
  //! largest reprentable value
  static T maxValue() throw() {
    return std::numeric_limits<T>::max();
  }
};

template<>
 struct NumericLimits<float>: public std::numeric_limits<float> {
  //! smallest reprentable value
  static float minValue() throw() {
    // floating-point values:
    // min encode smallest positive
    // value nearing 0 (in limit)
    return -std::numeric_limits<float>::max(); // this is the smallest
  }
  //! largest reprentable value
  static float maxValue() throw() {
    return std::numeric_limits<float>::max();
  }
};

template<>
 struct NumericLimits<double>: public std::numeric_limits<double> {
  //! smallest reprentable value
  static double minValue() throw() {
    // floating-point values:
    // min encode smallest positive
    // value nearing 0 (in limit)
    return -std::numeric_limits<double>::max(); // this is the smallest
  }
  //! largest reprentable value
  static double maxValue() throw() {
    return std::numeric_limits<double>::max();
  }
};

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
