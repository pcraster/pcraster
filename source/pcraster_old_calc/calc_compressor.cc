#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Compressor class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMPRESSOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMPRESSOR MEMBERS
//------------------------------------------------------------------------------

calc::Compressor::Compressor(const geo::RasterSpace& rs):
   d_rs(rs)
{
}



calc::Compressor::~Compressor()
{
}

const geo::RasterSpace& calc::Compressor::rasterSpace() const
{
  return d_rs;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



