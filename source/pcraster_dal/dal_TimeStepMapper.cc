#ifndef INCLUDED_DAL_TIMESTEPMAPPER
#include "dal_TimeStepMapper.h"
#define INCLUDED_DAL_TIMESTEPMAPPER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif



/*!
  \file
  This file contains the implementation of the TimeStepMapper class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMESTEPMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TIMESTEPMAPPER MEMBERS
//------------------------------------------------------------------------------

TimeStepMapper::TimeStepMapper()
{
  assert(!isValid());
}



TimeStepMapper::TimeStepMapper(
         double index,
         boost::posix_time::ptime const& time,
         boost::posix_time::time_duration const& duration)

  : d_index(index),
    d_time(time),
    d_duration(duration)

{
  assert(isValid());
}



// //! Copy constructor.
// TimeStepMapper::TimeStepMapper(
//          TimeStepMapper const& rhs)
// 
//   : d_index(rhs.d_index),
//     d_time(rhs.d_time),
//     d_duration(rhs.d_duration)
// 
// {
// }



TimeStepMapper::~TimeStepMapper()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
TimeStepMapper& TimeStepMapper::operator=(
         TimeStepMapper const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Improve, naive first try.
*/
TimeStepMapper& TimeStepMapper::operator|=(
         TimeStepMapper const& rhs)
{
  assert(isValid());
  assert(rhs.isValid());

  d_index = std::min<double>(d_index, rhs.d_index);
  d_duration = std::min<boost::posix_time::time_duration>(d_duration,
         rhs.d_duration);
  d_time = std::min<boost::posix_time::ptime>(d_time, rhs.d_time);

  assert(isValid());

  return *this;
}



double TimeStepMapper::index() const
{
  return d_index;
}



boost::posix_time::ptime TimeStepMapper::time() const
{
  return d_time;
}



boost::posix_time::time_duration const& TimeStepMapper::duration() const
{
  return d_duration;
}



bool TimeStepMapper::isValid() const
{
  return !d_time.is_not_a_date_time() && !d_duration.is_not_a_date_time() &&
         comparable(std::fmod(d_index, 1.0), 0.0);
}



double TimeStepMapper::source(
         boost::posix_time::ptime const& time) const
{
  assert(isValid());

  double result = d_index;
  boost::posix_time::time_iterator iter(d_time, d_duration);

  if(time >= d_time) {
    while(iter < time) {
      result += 1.0;
      ++iter;
    }
  }
  else {
    while(iter > time) {
      result -= 1.0;
      --iter;
    }
  }

  return result;
}



boost::posix_time::ptime TimeStepMapper::destination(
         double index) const
{
  assert(isValid());

  return d_time + d_duration * static_cast<int>(index - d_index);
}



bool TimeStepMapper::equals(
         TimeStepMapper const& mapper) const
{
  return d_index == mapper.d_index &&
         d_time == mapper.d_time &&
         d_duration == mapper.d_duration;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
std::ostream& operator<<(
         std::ostream& stream,
         TimeStepMapper const& mapper)
{
  stream
    << mapper.index()
    << " -> "
    << boost::posix_time::to_simple_string(mapper.time()) << ' '
    << boost::posix_time::to_simple_string(mapper.duration());

  return stream;
}
#endif



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

