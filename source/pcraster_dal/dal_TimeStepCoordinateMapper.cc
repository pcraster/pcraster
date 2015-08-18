#ifndef INCLUDED_DAL_TIMESTEPCOORDINATEMAPPER
#include "dal_TimeStepCoordinateMapper.h"
#define INCLUDED_DAL_TIMESTEPCOORDINATEMAPPER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif



/*!
  \file
  This file contains the implementation of the TimeStepCoordinateMapper class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMESTEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TIMESTEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

TimeStepCoordinateMapper::TimeStepCoordinateMapper(
         double index,
         boost::posix_time::ptime const& time,
         boost::posix_time::time_duration const& duration)

  : CoordinateMapper(),
    TimeStepMapper(index, time, duration)

{
}



TimeStepCoordinateMapper::TimeStepCoordinateMapper(
         TimeStepMapper const& mapper)

  : CoordinateMapper(),
    TimeStepMapper(mapper)

{
}



/* NOT IMPLEMENTED
//! Copy constructor.
TimeStepCoordinateMapper::TimeStepCoordinateMapper(
         TimeStepCoordinateMapper const& rhs)

  : Base(rhs)

{
}
*/



TimeStepCoordinateMapper::~TimeStepCoordinateMapper()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
TimeStepCoordinateMapper& TimeStepCoordinateMapper::operator=(
         TimeStepCoordinateMapper const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



std::string TimeStepCoordinateMapper::toString(
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
    assert(space.dimension(index).meaning() == dal::Time);

    size_t coordinate = address.coordinate<size_t>(index);
    boost::posix_time::ptime mappedCoordinate = destination(coordinate);

    // to_iso_extended_string make this xsd compatible and unambiguous
    result = boost::posix_time::to_iso_extended_string(mappedCoordinate);
  }

  return result;
}



bool TimeStepCoordinateMapper::equals(
         TimeStepCoordinateMapper const& mapper) const
{
  return TimeStepMapper::equals(mapper);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool operator==(
         TimeStepCoordinateMapper const& lhs,
         TimeStepCoordinateMapper const& rhs)
{
  return lhs.equals(rhs);
}



PCR_DAL_DECL bool operator!=(
         TimeStepCoordinateMapper const& lhs,
         TimeStepCoordinateMapper const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

