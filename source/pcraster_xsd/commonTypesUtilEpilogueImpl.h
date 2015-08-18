/*!
  \file
  This file is included in commonTypesXSD.cc

*/

// Library headers.
#ifndef INCLUDED_BOOST_DATE_TIME_POSIX_TIME_POSIX_TIME
#include <boost/date_time/posix_time/posix_time.hpp>
#define INCLUDED_BOOST_DATE_TIME_POSIX_TIME_POSIX_TIME
#endif

// PCRaster library headers.

// Module headers.






namespace pcrxsd {


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*! \brief xs:dateTime to boost::posix_time::ptime
 *
 * \precond
 *    dateTime is already validated as a valid xs:dateTime format
 *
 * This function may become obsolete when we implement custom mappings:
 *  http://wiki.codesynthesis.com/Tree/Customization_guide
 */
boost::posix_time::ptime toPosixTime(xml_schema::date_time const& dateTime) {

  using namespace boost::posix_time;
  return ptime(
      boost::gregorian::date(dateTime.year(),dateTime.month(),dateTime.day()),
      hours(dateTime.hours())+
      minutes(dateTime.minutes())+
      seconds(static_cast<long>(dateTime.seconds())));
}

/*! \brief pcrxml:TimeDuration to boost::posix_time::time_duration
 *
 * This function may become obsolete when we implement custom mappings:
 *  http://wiki.codesynthesis.com/Tree/Customization_guide
 */
boost::posix_time::time_duration toPosixTimeDuration(
    pcrxml::TimeDuration const& duration) {
  using namespace boost::posix_time;

  if (duration.hours().present())
    return time_duration(duration.hours().get(),0,0,0);
  if (duration.minutes().present())
    return time_duration(0,duration.minutes().get(),0,0);
  if (duration.seconds().present())
    return time_duration(0,0,duration.seconds().get(),0);
  return time_duration(0,0,0,0);
}


} // namespace pcrxsd

