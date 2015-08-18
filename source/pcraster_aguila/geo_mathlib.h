#ifndef INCLUDED_GEO_MATHLIB
#define INCLUDED_GEO_MATHLIB



#include <cassert>
#include <cmath>
#include "dev_MathUtils.h"



namespace geo {

template<class T>
inline void normalize(T* x, T* y, T* z)
{
  double l = dev::hypot(*x, *y, *z);

  assert(l > 0.0);

  *x /= l;
  *y /= l;
  *z /= l;
}

/*!
  \brief     Calculates the normal of the plane through three points.
  \param     x1
  \param     y1
  \param     z1
  \param     x2
  \param     y2
  \param     z2
  \param     x3
  \param     y3
  \param     z3
  \param     xn
  \param     yn
  \param     zn
  \return    .
  \exception .
  \warning   .
  \sa        .

  Given three points p1, p2 and p3 on a plane, the normal vector can be computed
  as the cross-product (p2 - p1) x (p3 - p1).

  The resulting normal is normalized by its length.
*/
template<class T>
inline void normal(T x1, T y1, T z1, T x2, T y2, T z2, T x3, T y3, T z3,
                   T *xn, T *yn, T *zn)
{
  *xn = (y2 - y1) * (z3 - z1) - (z2 - z1) * (y3 - y1);
  *yn = (z2 - z1) * (x3 - x1) - (x2 - x1) * (z3 - z1);
  *zn = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1);
  normalize(xn, yn, zn);
}

} // namespace geo

#endif
