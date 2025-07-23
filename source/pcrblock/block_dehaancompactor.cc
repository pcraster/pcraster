#include "stddefx.h"
#include "block_dehaancompactor.h"
#include "dal_MathUtils.h"

#include <cmath>


/*!
  \file
  This file contains the implementation of the DeHaanCompactor class.
*/



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DEHAANCOMPACTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DEHAANCOMPACTOR MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  All parameters are set to 0.0.
*/
DeHaanCompactor::DeHaanCompactor()

  : d_b(0.0),
    d_c(0.0),
    d_buoyancy(0.0)

{
}



//! Constructor.
/*!
  \param     b Parameter of compaction function to use.
  \param     c Parameter of compaction function to use.
  \param     buoyancy Parameter of compaction function to use.
*/
DeHaanCompactor::DeHaanCompactor(
         double b,
         double c,
         double buoyancy)

  : d_b(b),
    d_c(c),
    d_buoyancy(buoyancy)

{
}



//! Destructor.
/*!
*/
DeHaanCompactor::~DeHaanCompactor()
{
}



//! Returns the new thickness after compaction.
/*!
  \param     initialThickness
  \param     cummulativeLoad
  \param     duration
  \return    thickness
  \warning   \a initialThickness, \a cummulativeLoad and \a duration must all
             be larger or comparable to 0.0.
*/
REAL4 DeHaanCompactor::operator()(
         REAL4 initialThickness,
         REAL4 cummulativeLoad,
         double duration) const
{
  DEVELOP_PRECOND(dal::greaterOrComparable(initialThickness, REAL4(0.0)));
  DEVELOP_PRECOND(dal::greaterOrComparable(cummulativeLoad, REAL4(0.0)));
  DEVELOP_PRECOND(dal::greaterOrComparable(duration, 0.0));

  double result = 0.0;

  if(initialThickness > REAL4(0.0) && cummulativeLoad > REAL4(0.0) &&
         duration > REAL4(0.0)) {
    result = initialThickness *
         pow(cummulativeLoad / 0.05, -d_b) * pow(duration, -d_c);
  }

  DEVELOP_PRECOND(dal::greaterOrComparable(result, 0.0));
  DEVELOP_PRECOND(dal::smallerOrComparable(result, double(initialThickness)));

  return initialThickness - result;
}



//! Returns the buoyancy used.
/*!
  \return    buoyancy
*/
double DeHaanCompactor::buoyancy() const
{
  return d_buoyancy;
}



//! Returns whether *this equals \a compactor.
/*!
  \return    true or false
*/
bool DeHaanCompactor::equals(
         DeHaanCompactor const& compactor) const
{
  return d_b == compactor.d_b &&
         d_c == compactor.d_c &&
         d_buoyancy == compactor.d_buoyancy;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! Equality operator.
/*!
*/
bool operator==(
         DeHaanCompactor const& lhs,
         DeHaanCompactor const& rhs)
{
  return lhs.equals(rhs);
}



//! Inequality operator.
/*!
*/
bool operator!=(
         DeHaanCompactor const& lhs,
         DeHaanCompactor const& rhs)
{
  return !lhs.equals(rhs);
}



} // namespace block

