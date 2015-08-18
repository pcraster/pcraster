#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_VOXEL
#include "geo_voxel.h"
#define INCLUDED_GEO_VOXEL
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

geo::Voxel::Voxel(INT4 s)

  : d_sedType(s), d_thickness(0.0), d_origThickness(0.0)

{
}



geo::Voxel::Voxel(INT4 s, REAL8 t)

  : d_sedType(s), d_thickness(t), d_origThickness(t)

{
}



geo::Voxel::Voxel(INT4 s, REAL8 t, REAL8 ot)

  : d_sedType(s), d_thickness(t), d_origThickness(ot)

{
}



geo::Voxel::Voxel(const Voxel &rhs)

  : d_sedType(rhs.d_sedType),
    d_thickness(rhs.d_thickness), d_origThickness(rhs.d_origThickness)

{
}



geo::Voxel::~Voxel()
{
}



void geo::Voxel::clean()
{
}



geo::Voxel &geo::Voxel::operator=(const Voxel &rhs)
{
  if(this != &rhs)
  {
    d_sedType = rhs.d_sedType;
    d_thickness = rhs.d_thickness;
    d_origThickness = rhs.d_origThickness;
  }

  return *this;
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


