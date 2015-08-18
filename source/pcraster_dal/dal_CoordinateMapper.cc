#ifndef INCLUDED_DAL_COORDINATEMAPPER
#include "dal_CoordinateMapper.h"
#define INCLUDED_DAL_COORDINATEMAPPER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the CoordinateMapper class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
*/
CoordinateMapper::CoordinateMapper()
{
}



//! Destructor.
/*!
*/
CoordinateMapper::~CoordinateMapper()
{
}



//! Maps the coordinate of dimension \a index in \a address to the destination range.
/*!
  \param     space Data space of \a address.
  \param     address Address to change a coordinate of.
  \param     index Index of dimension to change coordinate of.
  \return    Updated coordinate in \a address.

  It is assumed that the coordinate of dimension \a index in \a address
  is in the source range.

  This default does nothing.
*/
void CoordinateMapper::mapToDestination(
         DataSpace const& /* space */,
         DataSpaceAddress& /* address */,
         size_t /* index */) const
{
}



//! Maps the coordinate of dimension \a index in \a address to the source range.
/*!
  \param     space Data space of \a address.
  \param     address Address to change a coordinate of.
  \param     index Index of dimension to change coordinate of.
  \return    Updated coordinate in \a address.

  It is assumed that the coordinate of dimension \a index in \a address
  is in the destination range.

  This default does nothing.
*/
void CoordinateMapper::mapToSource(
         DataSpace const& /* space */,
         DataSpaceAddress& /* address */,
         size_t /* index */) const
{
}



//! Returns a string representation of the coordinate of dimension \a index in \a address.
/*!
  \param     space Data space of \a address.
  \param     address Address to return coordinate for.
  \param     index Index of dimension to return coordinate for.
  \return    string

  It is assumed that the coordinate of dimension \a index in \a address
  is in the source range.

  The default returns the result of calling coordinateToString(DataSpace
  const&, DataSpaceAddress const&, size_t).
*/
std::string CoordinateMapper::toString(
         DataSpace const& space,
         DataSpaceAddress const& address,
         size_t index) const
{
  return coordinateToString(space, address, index);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

