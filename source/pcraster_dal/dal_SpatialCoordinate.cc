#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif

// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif



/*!
  \file
  This file contains the implementation of the SpatialCoordinate class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPATIALCOORDINATE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPATIALCOORDINATE MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Initializes the coordinates with 0, 0.
*/
SpatialCoordinate::SpatialCoordinate()

  : _x(0.0),
    _y(0.0)

{
}



//! Constructor.
/*!
  \param     x X-coordinate.
  \param     y Y-coordinate.
*/
SpatialCoordinate::SpatialCoordinate(
         double x,
         double y)

  : _x(x),
    _y(y)

{
}



//! Copy constructor.
/*!
  \param     rhs Object to copy construct from.
*/
SpatialCoordinate::SpatialCoordinate(
       SpatialCoordinate const& rhs)

  : _x(rhs._x),
    _y(rhs._y)

{
}



//! Destructor.
/*!
*/
SpatialCoordinate::~SpatialCoordinate()
{
}



SpatialCoordinate& SpatialCoordinate::operator=(
         SpatialCoordinate const& rhs)
{
  if(this != &rhs) {
    _x = rhs._x;
    _y = rhs._y;
  }

  return *this;
}



//! Returns whether *this equals \a rhs.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
bool SpatialCoordinate::equals(
         SpatialCoordinate const& rhs) const
{
  return comparable(_x, rhs._x) && comparable(_y, rhs._y);
}



//! Sets the x-coordinate to \a x.
/*!
  \param     x New x-coordinate.
*/
void SpatialCoordinate::setX(
         double x)
{
  _x = x;
}



//! Sets the y-coordinate to \a y.
/*!
  \param     y New y-coordinate.
*/
void SpatialCoordinate::setY(
         double y)
{
  _y = y;
}



//! Returns the x-coordinate.
/*!
  \return    X-coordinate.
*/
double SpatialCoordinate::x() const
{
  return _x;
}



//! Returns the y-coordinate.
/*!
  \return    Y-coordinate.
*/
double SpatialCoordinate::y() const
{
  return _y;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Equality operator.
/*!
  \param     lhs Left operand to compare.
  \param     rhs Right operand to compare.
  \return    true or false.
*/
PCR_DAL_DECL bool operator==(
         SpatialCoordinate const& lhs,
         SpatialCoordinate const& rhs)
{
  return lhs.equals(rhs);
}



//! Inequality operator.
/*!
  \param     lhs Left operand to compare.
  \param     rhs Right operand to compare.
  \return    true or false.
*/
PCR_DAL_DECL bool operator!=(
         SpatialCoordinate const& lhs,
         SpatialCoordinate const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

