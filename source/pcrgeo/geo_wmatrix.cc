#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_WMATRIX
#include "geo_wmatrix.h"
#define INCLUDED_GEO_WMATRIX
#endif

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

geo::WMatrix::WMatrix()
{
  d_m11 = d_m22 = d_m33 = 1.0;
  d_m12 = d_m13 = d_m21 = d_m23 = d_m31 = d_m32 = 0.0;
  d_dx = d_dy = d_dz = 0.0;
}



geo::WMatrix::WMatrix(const WMatrix &m)

  : d_m11(m.d_m11), d_m12(m.d_m12), d_m13(m.d_m13),
    d_m21(m.d_m21), d_m22(m.d_m22), d_m23(m.d_m23),
    d_m31(m.d_m31), d_m32(m.d_m32), d_m33(m.d_m33),
    d_dx(m.d_dx),   d_dy(m.d_dy),   d_dz(m.d_dz)

{
}



geo::WMatrix::WMatrix(REAL8 m11, REAL8 m12, REAL8 m13,
                      REAL8 m21, REAL8 m22, REAL8 m23,
                      REAL8 m31, REAL8 m32, REAL8 m33,
                      REAL8 dx,  REAL8 dy,  REAL8 dz)

  : d_m11(m11), d_m12(m12), d_m13(m13),
    d_m21(m21), d_m22(m22), d_m23(m23),
    d_m31(m31), d_m32(m32), d_m33(m33),
    d_dx(dx),   d_dy(dy),   d_dz(dz)

{
}



geo::WMatrix::~WMatrix()
{
}



geo::WMatrix &geo::WMatrix::operator=(const WMatrix &rhs)
{
  if(this != &rhs)
  {
    d_m11 = rhs.d_m11; d_m12 = rhs.d_m12; d_m13 = rhs.d_m13;
    d_m21 = rhs.d_m21; d_m22 = rhs.d_m22; d_m23 = rhs.d_m23;
    d_m31 = rhs.d_m31; d_m32 = rhs.d_m32; d_m33 = rhs.d_m33;
    d_dx  = rhs.d_dx;  d_dy  = rhs.d_dy;  d_dz  = rhs.d_dz;
  }

  return *this;
}



void geo::WMatrix::reset()
{
  d_m11 = d_m22 = d_m33 = 1.0;
  d_m12 = d_m13 = d_m21 = d_m23 = d_m31 = d_m32 = 0.0;
  d_dx = d_dy = d_dz = 0.0;
}



void geo::WMatrix::setMatrix(REAL8 m11, REAL8 m12, REAL8 m13,
                             REAL8 m21, REAL8 m22, REAL8 m23,
                             REAL8 m31, REAL8 m32, REAL8 m33,
                             REAL8 dx, REAL8 dy, REAL8 dz)
{
  d_m11 = m11;
  d_m12 = m12;
  d_m13 = m13;
  d_m21 = m21;
  d_m22 = m22;
  d_m23 = m23;
  d_m31 = m31;
  d_m32 = m32;
  d_m33 = m33;
  d_dx  = dx;
  d_dy  = dy; 
  d_dz  = dz;
}



void geo::WMatrix::bmul(const WMatrix &m)
{
  setMatrix(m.d_m11 * d_m11 + m.d_m12 * d_m21 + m.d_m13 * d_m31,
            m.d_m11 * d_m12 + m.d_m12 * d_m22 + m.d_m13 * d_m32,
            m.d_m11 * d_m13 + m.d_m12 * d_m23 + m.d_m13 * d_m33,
            m.d_m21 * d_m11 + m.d_m22 * d_m21 + m.d_m23 * d_m31,
            m.d_m21 * d_m12 + m.d_m22 * d_m22 + m.d_m23 * d_m32,
            m.d_m21 * d_m13 + m.d_m22 * d_m23 + m.d_m23 * d_m33,
            m.d_m31 * d_m11 + m.d_m32 * d_m21 + m.d_m33 * d_m31,
            m.d_m31 * d_m12 + m.d_m32 * d_m22 + m.d_m33 * d_m32,
            m.d_m31 * d_m13 + m.d_m32 * d_m23 + m.d_m33 * d_m33,
            m.d_dx  * d_m11 + m.d_dy  * d_m21 + m.d_dz  * d_m31 + d_dx,
            m.d_dx  * d_m12 + m.d_dy  * d_m22 + m.d_dz  * d_m32 + d_dy,
            m.d_dx  * d_m13 + m.d_dy  * d_m23 + m.d_dz  * d_m33 + d_dz);
}



void geo::WMatrix::map(REAL8 x, REAL8 y, REAL8 z,
                       REAL8 *tx, REAL8 *ty, REAL8 *tz)
{
  *tx = d_m11 * x + d_m21 * y + d_m31 * z + d_dx;
  *ty = d_m12 * x + d_m22 * y + d_m32 * z + d_dy;
  *tz = d_m13 * x + d_m23 * y + d_m33 * z + d_dz;
}



void geo::WMatrix::translate(REAL8 dx, REAL8 dy, REAL8 dz)
{
  WMatrix result(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, dx, dy, dz);
  bmul(result);
}



void geo::WMatrix::translate(const Point<REAL8, 3> &p)
{
  translate(p[0], p[1], p[2]);
}



void geo::WMatrix::scale(REAL8 sx, REAL8 sy, REAL8 sz)
{
  WMatrix result(sx, 0.0, 0.0, 0.0, sy, 0.0, 0.0, 0.0, sz, 0.0, 0.0, 0.0);
  bmul(result);
}



void geo::WMatrix::scale(const Point<REAL8, 3> &p)
{
  scale(p[0], p[1], p[2]);
}



static const REAL8 deg2rad = 0.017453292519943295769; // pi/180

void geo::WMatrix::rotate(REAL8 rx, REAL8 ry, REAL8 rz)
{
  REAL8 rxRad = deg2rad * rx;
  REAL8 ryRad = deg2rad * ry;
  REAL8 rzRad = deg2rad * rz;

  REAL8 sinx = std::sin(rxRad);
  REAL8 cosx = std::cos(rxRad);
  REAL8 siny = std::sin(ryRad);
  REAL8 cosy = std::cos(ryRad);
  REAL8 sinz = std::sin(rzRad);
  REAL8 cosz = std::cos(rzRad);

  WMatrix rm;
  // Rotation around z-as.
  rm.setMatrix(cosx, sinx, 0.0, -sinx, cosx, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0);
  bmul(rm);

  // Rotation around x-as.
  rm.setMatrix(1.0, 0.0, 0.0, 0.0, cosy, siny, 0.0, -siny, cosy, 0.0, 0.0, 0.0);
  bmul(rm);

  // Rotation around y-as.
  rm.setMatrix(cosz, 0.0, -sinz, 0.0, 1.0, 0.0, sinz, 0.0, cosz, 0.0, 0.0, 0.0);
  bmul(rm);
}



void geo::WMatrix::rotate(const Point<REAL8, 3> &p)
{
  rotate(p[0], p[1], p[2]);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


