#ifndef INCLUDED_DAL_SPACEDIMENSIONS
#include "dal_SpaceDimensions.h"
#define INCLUDED_DAL_SPACEDIMENSIONS
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
  This file contains the implementation of the SpaceDimensions class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPACEDIMENSIONS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPACEDIMENSIONS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     west Western most coordinate.
  \param     north Northern most coordinate.
  \param     east Eastern most coordinate.
  \param     south Southern most coordinate.
  \warning   It is assumed \a west <= \a east and south <= north.
*/
SpaceDimensions::SpaceDimensions(
         double west,
         double north,
         double east,
         double south)

  : _northWest(west, north),
    _southEast(east, south)

{
  assert(dal::smallerOrComparable(_northWest.x(), _southEast.x()));
  assert(dal::smallerOrComparable(_southEast.y(), _northWest.y()));
}



//! Constructor.
/*!
  \param     northWest North-west coordinate.
  \param     southEast South-east coordinate.
  \warning   It is assumed \a west <= \a east and south <= north.
*/
SpaceDimensions::SpaceDimensions(
         SpatialCoordinate const& northWest,
         SpatialCoordinate const& southEast)

  : _northWest(northWest),
    _southEast(southEast)

{
  assert(dal::smallerOrComparable(_northWest.x(), _southEast.x()));
  assert(dal::smallerOrComparable(_southEast.y(), _northWest.y()));
}



//! Copy constructor.
/*!
  \param     rhs Object to copy from.
*/
SpaceDimensions::SpaceDimensions(
         SpaceDimensions const& rhs)

  : _northWest(rhs._northWest),
    _southEast(rhs._southEast)

{
}



//! Destructor.
/*!
*/
SpaceDimensions::~SpaceDimensions()
{
}



//! Assignment operator.
/*!
  \param     rhs Object to copy from.
  \return    Reference to *this.
*/
SpaceDimensions const& SpaceDimensions::operator=(
         SpaceDimensions const& rhs)
{
  if(this != &rhs) {
    _northWest = rhs._northWest;
    _southEast = rhs._southEast;
  }

  return *this;
}



//! Merges the properties of \a rhs with *this.
/*!
  \param     rhs Object to merge with.
  \return    Reference to *this.

  The object is updated to include the spatial area of \a rhs.
*/
SpaceDimensions const& SpaceDimensions::operator|=(
         SpaceDimensions const& rhs)
{
  if(this != &rhs) {
    _northWest = SpatialCoordinate(
         std::min(_northWest.x(), rhs._northWest.x()),
         std::max(_northWest.y(), rhs._northWest.y()));
    _southEast = SpatialCoordinate(
         std::max(_southEast.x(), rhs._southEast.x()),
         std::min(_southEast.y(), rhs._southEast.y()));
  }

  return *this;
}



//! Returns whether \a rhs equals *this.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
bool SpaceDimensions::equals(
         SpaceDimensions const& rhs) const
{
   return _northWest == rhs._northWest && _southEast == rhs._southEast;
}



double SpaceDimensions::north() const
{
  return _northWest.y();
}



double SpaceDimensions::west() const
{
  return _northWest.x();
}



double SpaceDimensions::south() const
{
  return _southEast.y();
}



double SpaceDimensions::east() const
{
  return _southEast.x();
}



//! Returns the amount of world coordinate units between the west and east borders.
/*!
  \return    Extent.
  \todo      Test.
*/
double SpaceDimensions::longitudinalExtent() const
{
  return _southEast.x() - _northWest.x();
}



//! Returns the amount of world coordinate units between the north and south borders.
/*!
  \return    Extent.
  \todo      Test.
*/
double SpaceDimensions::latitudinalExtent() const
{
  return _northWest.y() - _southEast.y();
}



double SpaceDimensions::area() const
{
  return longitudinalExtent() * latitudinalExtent();
}



bool SpaceDimensions::contains(
         double x,
         double y) const
{
  return greaterOrComparable(x, _northWest.x()) &&
         smallerOrComparable(x, _southEast.x()) &&
         greaterOrComparable(y, _southEast.y()) &&
         smallerOrComparable(y, _northWest.y());
}



bool SpaceDimensions::contains(
         SpatialCoordinate const& coordinate) const
{
  return contains(coordinate.x(), coordinate.y());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Equality operator.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
PCR_DAL_DECL bool operator==(
         SpaceDimensions const& lhs,
         SpaceDimensions const& rhs)
{
  return lhs.equals(rhs);
}



//! Inequality operator.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
PCR_DAL_DECL bool operator!=(
         SpaceDimensions const& lhs,
         SpaceDimensions const& rhs)
{
  return !lhs.equals(rhs);
}



PCR_DAL_DECL SpaceDimensions operator|(
         SpaceDimensions const& lhs,
         SpaceDimensions const& rhs)
{
  return SpaceDimensions(lhs) |= rhs;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

