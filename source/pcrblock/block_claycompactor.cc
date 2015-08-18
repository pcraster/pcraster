#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_CLAYCOMPACTOR
#include "block_claycompactor.h"
#define INCLUDED_BLOCK_CLAYCOMPACTOR
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ClayCompactor class.
*/



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLAYCOMPACTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLAYCOMPACTOR MEMBERS
//------------------------------------------------------------------------------

ClayCompactor::ClayCompactor()
{
}



ClayCompactor::~ClayCompactor()
{
}



REAL4 ClayCompactor::operator()(
         REAL4 originalThickness,
         REAL4 depth) const
{
  REAL4 result = static_cast<REAL4>(
         (1 - 0.043 * std::log(static_cast<double>(depth) + 1) -
         0.0054 * std::pow(std::log(static_cast<double>(depth) + 1), 2.0)) *
         originalThickness);

  DEVELOP_POSTCOND(result > 0.0);

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace block

