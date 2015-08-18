#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_RIMAP
#include "com_rimap.h"
#define INCLUDED_COM_RIMAP
#endif

#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
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

com::RIMap::RIMap()

  : d_r1(0.0), d_r2(0.0), d_i1(0), d_i2(0), d_conv(0.0)

{
}



com::RIMap::RIMap(REAL8 r1, REAL8 r2, int i1, int i2)
{
  setRealRange(r1, r2);
  setIntRange(i1, i2);
}



com::RIMap::~RIMap()
{
}



void com::RIMap::recalc()
{
  if(d_r1 != d_r2)
    d_conv = static_cast<REAL8>(d_i2 - d_i1) / (d_r2 - d_r1);
  else
    d_conv = 0.0;
}



void com::RIMap::setRealRange(REAL8 r1, REAL8 r2)
{
  d_r1 = r1;
  d_r2 = r2;
  recalc();
}



void com::RIMap::setIntRange(int i1, int i2)
{
  d_i1 = i1;
  d_i2 = i2;
  recalc();
}



REAL8 com::RIMap::r1() const
{
  return d_r1;
}



REAL8 com::RIMap::r2() const
{
  return d_r2;
}



int com::RIMap::i1() const
{
  return d_i1;
}



int com::RIMap::i2() const
{
  return d_i2;
}



int com::RIMap::transform(REAL8 v) const
{
  return d_i1 + boost::math::iround((v - d_r1) * d_conv);
}



REAL8 com::RIMap::transform(int v) const
{
  if(d_conv == 0.0)
    return 0.0;
  else
    return d_r1 + static_cast<double>(v - d_i1) / d_conv;
}



bool com::RIMap::inRange(REAL8 r) const
{
  return d_r1 <= r && r <= d_r2;
}



bool com::RIMap::inRange(int i) const
{
  return d_i1 <= i && i <= d_i2;
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


