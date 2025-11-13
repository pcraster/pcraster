#include "dal_SpaceStepCoordinateMapper.h"
#include "dal_DataSpace.h"
#include "dal_DataSpaceAddress.h"

#include <sstream>


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

    size_t const coordinate = address.coordinate<size_t>(index);
    double const mappedCoordinate = destination(coordinate);

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

