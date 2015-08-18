#ifndef INCLUDED_DAL_SPACESTEPCOORDINATEMAPPER
#include "dal_SpaceStepCoordinateMapper.h"
#define INCLUDED_DAL_SPACESTEPCOORDINATEMAPPER
#endif

// External headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif



/*!
  \file
  This file contains the implementation of the SpaceStepCoordinateMapper class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPACESTEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPACESTEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

SpaceStepCoordinateMapper::SpaceStepCoordinateMapper(
         size_t index,
         double coordinate,
         double cellSize)

  : CoordinateMapper(),
    SpaceStepMapper(index, coordinate, cellSize)

{
}



SpaceStepCoordinateMapper::SpaceStepCoordinateMapper(
         SpaceStepMapper const& mapper)

  : CoordinateMapper(),
    SpaceStepMapper(mapper)

{
}



SpaceStepCoordinateMapper::~SpaceStepCoordinateMapper()
{
}



std::string SpaceStepCoordinateMapper::toString(
         DataSpace const&
#ifdef DEBUG_BUILD
           space
#endif
         ,
         DataSpaceAddress const& address,
         size_t index) const
{
  std::string result = "unset";

  if(address.isValid(index)) {
    assert(space.dimension(index).meaning() == Space);

    size_t coordinate = address.coordinate<size_t>(index);
    double mappedCoordinate = destination(coordinate);

    std::stringstream stream;
    stream << mappedCoordinate; // << std::ends;
    result = stream.str();
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

