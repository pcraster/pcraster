#include "stddefx.h"
#include "block_sandcompactor.h"
#include "dal_MathUtils.h"

#include <cmath>
#include <numbers>


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

  auto result = static_cast<REAL4>(
    (0.51 + 0.49 * std::pow(std::numbers::e, -static_cast<double>(depth) / 3700.0)) *
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

