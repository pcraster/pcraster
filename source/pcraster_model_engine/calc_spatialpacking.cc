#include "stddefx.h"
#include "calc_spatialpacking.h"



/*!
  \file
  This file contains the implementation of the SpatialPacking class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPATIALPACKING MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPATIALPACKING MEMBERS
//------------------------------------------------------------------------------

calc::SpatialPacking::SpatialPacking(const geo::RasterDim& rd):
   d_rd(rd)
{
}



calc::SpatialPacking::~SpatialPacking()
{
}

const geo::RasterDim& calc::SpatialPacking::rasterDim() const
{
  return d_rd;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



