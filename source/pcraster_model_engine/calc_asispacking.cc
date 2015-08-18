#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASISPACKING
#include "calc_asispacking.h"
#define INCLUDED_CALC_ASISPACKING
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif


/*!
  \file
  This file contains the implementation of the AsIsPacking class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASISPACKING MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASISPACKING MEMBERS
//------------------------------------------------------------------------------

calc::AsIsPacking::AsIsPacking(const geo::RasterDim& rs):
  SpatialPacking(rs)
{
}



calc::AsIsPacking::~AsIsPacking()
{
}

const calc::Field* calc::AsIsPacking::unpack(const Field* f)const
{
  return f;
}

calc::Field* calc::AsIsPacking::pack(const Field *f) const
{
  return (Field *)f;
}


calc::Field   *calc::AsIsPacking::createSpatial(VS vs) const
{

  return new Spatial(vs,CRI_X,rasterDim().nrCells());
}

size_t calc::AsIsPacking::toRasterId(size_t fieldId) const
{
  return fieldId;
}

size_t calc::AsIsPacking::toFieldId(size_t rasterId) const
{
  return rasterId;
}

size_t calc::AsIsPacking::nrFieldCells() const
{
  return rasterDim().nrCells();
}

calc::AsIsPacking* calc::AsIsPacking::createClone() const
{
  return new AsIsPacking(*this);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



