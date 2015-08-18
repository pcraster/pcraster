#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_SANDCOMPACTOR
#include "block_sandcompactor.h"
#define INCLUDED_BLOCK_SANDCOMPACTOR
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

// PCRaster library headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the SandCompactor class.
*/



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SANDCOMPACTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SANDCOMPACTOR MEMBERS
//------------------------------------------------------------------------------

SandCompactor::SandCompactor()
{
}



SandCompactor::~SandCompactor()
{
}



REAL4 SandCompactor::operator()(
         REAL4 originalThickness,
         REAL4 depth)
{
  DEVELOP_PRECOND(dal::greaterOrComparable(originalThickness, REAL4(0.0)));
  DEVELOP_PRECOND(dal::greaterOrComparable(depth, REAL4(0.0)));

  REAL4 result = static_cast<REAL4>(
    (0.51 + 0.49 * std::pow(2.7182818, -static_cast<double>(depth) / 3700.0)) *
    originalThickness);

  DEVELOP_PRECOND(dal::greaterOrComparable(result, REAL4(0.0)));

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace block

