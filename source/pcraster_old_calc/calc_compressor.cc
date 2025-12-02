#include "stddefx.h"
#include "calc_compressor.h"

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

calc::Compressor::Compressor(const geo::RasterSpace &rs) : d_rs(rs)
{
}

calc::Compressor::~Compressor()
{
}

const geo::RasterSpace &calc::Compressor::rasterSpace() const
{
  return d_rs;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
