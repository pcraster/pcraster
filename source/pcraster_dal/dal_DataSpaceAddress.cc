#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DataSpaceAddress class.
*/

namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEADDRESS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEADDRESS MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Creates an address with no coordinates. Such an address is only useful with
  a data space without dimensions (empty). In that case there is no data space
  with addresses where data sets might be defined. But still the name of a
  data set might be used in combination with an empty data space and address.
*/
DataSpaceAddress::DataSpaceAddress()
{
}



//! Constructor.
/*!
  \param     size Initial size of the address (number of dimensions).

  Room for \a size coordinates is reserved. isValid(size_t) returns false for
  those coordinates. Use setCoordinate<T>(size_t, T const&) to set each
  coordinate to a valid value.
*/
DataSpaceAddress::DataSpaceAddress(
         size_t size)

  : d_coordinates(size)

{
}



//! Destructor.
/*!
*/
DataSpaceAddress::~DataSpaceAddress()
{
}



//! Unsets the coordinate with index \a index.
/*!
  \param     index Index of (the dimension of) the coordinate to unset.
  \warning   A coordinate with index \index must already exist.

  For an unset coordinate isValid(size_t) will return false.
*/
void DataSpaceAddress::unsetCoordinate(
         size_t index)
{
  assert(index < d_coordinates.size());
  d_coordinates[index] = boost::any();
}



//! Erases the coordinate with index \a index.
/*!
  \param     index Index of (the dimension of) the coordinate to erase.
  \warning   A coordinate with index \index must already exist. The size of
             the address will become one less than before.
*/
void DataSpaceAddress::eraseCoordinate(size_t index)
{
  assert(index < d_coordinates.size());
  d_coordinates.erase(d_coordinates.begin() + index);
}



void DataSpaceAddress::resize(
         size_t size)
{
  d_coordinates.resize(size);
}



//! Erases all coordinates.
/*!
*/
void DataSpaceAddress::clear()
{
  d_coordinates.clear();
}



//! Returns the number of coordinates.
/*!
  \return    Number
*/
size_t DataSpaceAddress::size() const
{
  return d_coordinates.size();
}



//! Returns whether coordinate \a index is valid.
/*!
  \param     index Index of (the dimension of) the coordinate to check.
  \return    true or false

  A coordinate is valid if a value is set for it.
*/
bool DataSpaceAddress::isValid(
         size_t index) const
{
  assert(index < d_coordinates.size());

  return !d_coordinates[index].empty();
}



//! Returns whether one of the coordinates is valid.
/*!
  \return    true or false
  \todo      Trace calls to this function and rethink it. Returns true when
             at least one coordinate is valid. I would thing it returns true
             when all coordinates are valid. Also, an empty address should be
             invalid, it is useless for pointing at a location in a data space.

  This function also returns true when size() is 0. In all other cases
  this function returns false.

  A coordinate is valid if a value is set for it.
*/
bool DataSpaceAddress::isValid() const
{
  for(size_t i = 0; i < d_coordinates.size(); ++i) {
    if(isValid(i)) {
      return true;
    }
  }

  return size() == 0;
}



size_t DataSpaceAddress::nrInvalidCoordinates() const
{
  size_t result = 0;

  for(size_t i = 0; i < d_coordinates.size(); ++i) {
    if(!isValid(i)) {
      ++result;
    }
  }

  return result;
}



//! Returns the coordinate with index \a index.
/*!
  \param     index Index of (the dimension of) the coordinate to return.
  \return    boost::any object with the coordinate.
  \warning   No guarantee is made regarding the validity of the returned
             coordinate.
  \todo      Check uses of this function and remove it from the interface.
             The boost::any return value is an implementation detail.
*/
boost::any const& DataSpaceAddress::coordinate(
         size_t index) const
{
  assert(index < d_coordinates.size());

  return d_coordinates[index];
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal
