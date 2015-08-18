#ifndef INCLUDED_DAL_STEPCOORDINATEMAPPER
#include "dal_StepCoordinateMapper.h"
#define INCLUDED_DAL_STEPCOORDINATEMAPPER
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif



/*!
  \file
  This file contains the implementation of the StepCoordinateMapper class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC STEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

StepCoordinateMapper::StepCoordinateMapper(
         double sourceFirstStep,
         double sourceLastStep,
         double destinationFirstStep,
         double destinationLastStep,
         MissingDataStrategy strategy)

  : CoordinateMapper(),
    StepMapper(
         sourceFirstStep, sourceLastStep,
         destinationFirstStep, destinationLastStep),
    d_missingDataStrategy(strategy)

{
  assert(strategy == SetToMissingValue ||
         strategy == UsePrevious ||
         strategy == UseNearest);
}



StepCoordinateMapper::StepCoordinateMapper(
         StepMapper const& mapper,
         MissingDataStrategy strategy)

  : CoordinateMapper(),
    StepMapper(mapper),
    d_missingDataStrategy(strategy)

{
  assert(strategy == SetToMissingValue ||
         strategy == UsePrevious ||
         strategy == UseNearest);
}



StepCoordinateMapper::~StepCoordinateMapper()
{
}



void StepCoordinateMapper::mapToDestination(
         DataSpace const& /* space */,
         DataSpaceAddress& address,
         size_t index) const
{
  if(address.isValid(index)) {
    double result = destination(address.coordinate<size_t>(index));

    if(result < 1.0) {
      address.unsetCoordinate(index);
    }
    else {
      if(!comparable(std::fmod(result, 1.0), 0.0)) {
        if(d_missingDataStrategy == SetToMissingValue) {
          address.unsetCoordinate(index);
        }
        else if(d_missingDataStrategy == UsePrevious) {
          address.setCoordinate<size_t>(index,
              static_cast<size_t>(floor(result)));
        }
        else if(d_missingDataStrategy == UseNearest) {
          address.setCoordinate<size_t>(index, round<double, size_t>(result));
        }
      }
      else {
        address.setCoordinate<size_t>(index, static_cast<size_t>(result));
      }
    }
  }
}



void StepCoordinateMapper::mapToSource(
         DataSpace const& /* space */,
         DataSpaceAddress& address,
         size_t index) const
{
  if(address.isValid(index)) {
    double result = source(address.coordinate<size_t>(index));

    if(result < 1.0) {
      address.unsetCoordinate(index);
    }
    else {
      assert(comparable(std::fmod(result, 1.0), 0.0));
      address.setCoordinate<size_t>(index, static_cast<size_t>(result));
    }
  }
}



std::string StepCoordinateMapper::toString(
         DataSpace const&
#ifdef DEBUG_DEVELOP
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

    // result = boost::lexical_cast<std::string>(mappedCoordinate);
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

