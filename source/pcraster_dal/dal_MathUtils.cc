#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the MathUtils class.
*/



namespace dal {

template<typename T>
inline void mergeRanges(
         T& first,
         T& last,
         T& step,
         T const& first2,
         T const& last2,
         T const& step2)
{
  assert(smallerOrComparable(first, last));
  assert(step > T(0));
  assert(smallerOrComparable(first2, last2));
  assert(step2 > T(0));

  if(comparable(first, last) && valueInRange(first2, last2, step2, first)) {
    first = first2;
    last = last2;
    step = step2;
  }
  else if(comparable(first2, last2) &&
         valueInRange(first, last, step, first2)) {
    // Nothing to do.
  }
  else {
    first = std::min<>(first, first2);
    last = std::max<>(last, last2);

    if(comparable(first, first2)) {
      step = gcd<T>(step, step2);
    }
    else if(first > first2) {
      step = gcd<T>(step, first - first2);
    }
    else {
      step = gcd<T>(step, first2 - first);
    }
  }
}



template<typename T>
T gcdFloat(
         T a,
         T b)
{
  return static_cast<T>(
         boost::math::gcd(
              round<T, INT4>(a * T(1e3)),
              round<T, INT4>(b * T(1e3)))
         ) / T(1e3);
}

template
void mergeRanges<unsigned long long>(
         unsigned long long& first,
         unsigned long long& last,
         unsigned long long& step,
         unsigned long long const& first2,
         unsigned long long const& last2,
         unsigned long long const& step2);

template
void mergeRanges<long long>(
         long long& first,
         long long& last,
         long long& step,
         long long const& first2,
         long long const& last2,
         long long const& step2);


template
void mergeRanges<unsigned long>(
         unsigned long& first,
         unsigned long& last,
         unsigned long& step,
         unsigned long const& first2,
         unsigned long const& last2,
         unsigned long const& step2);
template
void mergeRanges<UINT4>(
         UINT4& first,
         UINT4& last,
         UINT4& step,
         UINT4 const& first2,
         UINT4 const& last2,
         UINT4 const& step2);
template
void mergeRanges<INT4>(
         INT4& first,
         INT4& last,
         INT4& step,
         INT4 const& first2,
         INT4 const& last2,
         INT4 const& step2);
template
void mergeRanges<REAL4>(
         REAL4& first,
         REAL4& last,
         REAL4& step,
         REAL4 const& first2,
         REAL4 const& last2,
         REAL4 const& step2);
template
void mergeRanges<REAL8>(
         REAL8& first,
         REAL8& last,
         REAL8& step,
         REAL8 const& first2,
         REAL8 const& last2,
         REAL8 const& step2);

template
float gcdFloat<REAL4>(
         REAL4 a,
         REAL4 b);
template
REAL8 gcdFloat<REAL8>(
         REAL8 a,
         REAL8 b);

} // namespace dal

