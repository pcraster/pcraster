#ifndef INCLUDED_DAL_DATASPACEADDRESSMAPPER
#include "dal_DataSpaceAddressMapper.h"
#define INCLUDED_DAL_DATASPACEADDRESSMAPPER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_COORDINATEMAPPER
#include "dal_CoordinateMapper.h"
#define INCLUDED_DAL_COORDINATEMAPPER
#endif



/*!
  \file
  This file contains the implementation of the DataSpaceAddressMapper class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEADDRESSMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEADDRESSMAPPER MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  The layered data space will be empty and no coordinate mappers will be set.
*/
DataSpaceAddressMapper::DataSpaceAddressMapper()
{
}



//! Constructor.
/*!
  \param     space Data space of addresses used.

  Default mappers (dummies) are installed for each dimension in \a space.
*/
DataSpaceAddressMapper::DataSpaceAddressMapper(
         DataSpace const& space)

  : _space(space)

{
  initMappers();
}



//! Destructor.
/*!
*/
DataSpaceAddressMapper::~DataSpaceAddressMapper()
{
  deleteMappers();
}



//! Installs default mappers (dummies) for each dimension in the layered data space.
/*!
*/
void DataSpaceAddressMapper::initMappers()
{
  assert(_mappers.empty());

  _mappers.resize(_space.rank(), 0);

  for(size_t i = 0; i < _space.rank(); ++i) {
    // Install dummy mapper.
    setMapper(i, new CoordinateMapper());
  }
}



//! Deletes the currently installed coordinate mappers and clears the collection.
/*!
*/
void DataSpaceAddressMapper::deleteMappers()
{
  for(size_t i = 0; i < _space.rank(); ++i) {
    delete _mappers[i];
  }

  _mappers.clear();
}



//! Changes the layered data space to \a space.
/*!
  \param     space New space to use.
  \todo      Might need to be adjusted to keep mappers for dimensions present
             in both current and new space.

  Currently installed coordinate mappers are deleted first. Default coordinate
  mappers (dummies) are installed for each dimension in the new data space.
*/
void DataSpaceAddressMapper::setDataSpace(
         DataSpace const& space)
{
  deleteMappers();
  _space = space;
  initMappers();
}



//! Sets a new coordinate mapper for dimension with index \a index.
/*!
  \param     index Index of dimension to change mapper for.
  \param     mapper New coordinate mapper.
  \warning   The \a mapper passed in must be ours to delete.

  The currently set mapper is deleted first.
*/
void DataSpaceAddressMapper::setMapper(
         size_t index,
         CoordinateMapper* mapper)
{
  assert(index < _mappers.size());
  assert(mapper);

  if(_mappers[index]) {
    delete _mappers[index];
  }

  _mappers[index] = mapper;
}



//! Returns the layered data space.
/*!
  \return    Data space.
*/
DataSpace const& DataSpaceAddressMapper::space() const
{
  return _space;
}



//! Returns the collection of coordinate mappers.
/*!
  \return    Mappers.
*/
std::vector<CoordinateMapper*> const& DataSpaceAddressMapper::mappers() const
{
  return _mappers;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
CoordinateMapper const* DataSpaceAddressMapper::mapper(
         size_t index) const
{
  return _mappers[index];
}



//! Maps the coordinates in \a address from the source dimension ranges to the destination dimension ranges and returns the result.
/*!
  \param     address Address to map coordinates for.
  \return    Resulting data space address.
  \warning   \a address must be a valid address in the layered data space.
*/
DataSpaceAddress DataSpaceAddressMapper::destination(
         DataSpaceAddress const& address) const
{
  DataSpaceAddress result(address);

  for(size_t i = 0; i < space().rank(); ++i) {
    mappers()[i]->mapToDestination(space(), result, i);
  }

  return result;
}



//! Maps the coordinates in \a address from the destination dimension ranges to the source dimension ranges and returns the result.
/*!
  \param     address Address to map coordinates for.
  \return    Resulting data space address.
  \warning   \a address must be a valid address in the layered data space.
*/
DataSpaceAddress DataSpaceAddressMapper::source(
         DataSpaceAddress const& address) const
{
  DataSpaceAddress result(address);

  for(size_t i = 0; i < space().rank(); ++i) {
    mappers()[i]->mapToSource(space(), result, i);
  }

  return result;
}



//! Returns a string representation of the coordinates in \a address.
/*!
  \param     address Address to use.
  \return    string

  It is assumed that the coordinates of the \a address passed in are in
  source dimensions ranges. Coordinates are mapped to destination ranges
  first. The result will be a string representation of the coordinates
  in the destination ranges.
*/
std::string DataSpaceAddressMapper::toString(
         DataSpaceAddress const& address) const
{
  std::string result;

  for(size_t i = 0; i < space().rank(); ++i) {
    result += "/" + mappers()[i]->toString(space(), address, i);
  }

  return result;
}



//! Returns a string representation of the coordinate for dimension \a index in \a address.
/*!
  \param     address Address to use.
  \param     index Index of dimension to use.
  \return    string

  It is assumed that the coordinates of the \a address passed in are in
  source dimensions ranges. The result will be a string represenation
  of the coordinate in the destination range.
*/
std::string DataSpaceAddressMapper::toString(
         DataSpaceAddress const& address,
         size_t index) const
{
  return mappers()[index]->toString(space(), address, index);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

