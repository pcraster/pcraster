#ifndef INCLUDED_PCRXSD_COMMONTYPESTEST
#include "pcrxsd_commontypestest.h"
#define INCLUDED_PCRXSD_COMMONTYPESTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_BOOST_DATE_TIME_POSIX_TIME_PTIME
#include "boost/date_time/posix_time/ptime.hpp"
#define INCLUDED_BOOST_DATE_TIME_POSIX_TIME_PTIME
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COMMONTYPESXSD
#include "commonTypesXSD.h"
#define INCLUDED_COMMONTYPESXSD
#endif
#ifndef INCLUDED_UNITTESTXSD
#include "unitTestXSD.h"
#define INCLUDED_UNITTESTXSD
#endif
#ifndef INCLUDED_PCRXSD_UTILS
#include "pcrxsd_utils.h"
#define INCLUDED_PCRXSD_UTILS
#endif

/*!
  \file
  This file contains the implementation of the CommonTypesTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace pcrxsd {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMONTYPES MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*CommonTypesTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CommonTypesTest> instance(new CommonTypesTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CommonTypesTest::testToBoostPosixTime, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommonTypesTest::testTimeDurationAssumption, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommonTypesTest::testTimeDuration, instance));

  return suite;
}

//! make a date_time and validate first
static xml_schema::date_time makeDateTime(const std::string& date)
{
  std::ostringstream o; 
  o << "<pcr:clock                                                   \
    xmlns:pcr='http://www.pcraster.nl/pcrxml'                        \
    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'            \
    xsi:schemaLocation='http://www.pcraster.nl/pcrxml unitTest.xsd'>";
  o << date << "</pcr:clock> ";
  std::istringstream i(o.str());
  std::auto_ptr<xml_schema::date_time> d(pcrxml::clock(i));
  return *d;
}

//! make a date_time and validate first
static pcrxml::TimeDuration makeTimeDuration(
    const std::string& duration)
{
  std::ostringstream o; 
  o << "<pcr:unitTestOnlyDuration                                   \
    xmlns:pcr='http://www.pcraster.nl/pcrxml'                        \
    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'            \
    xsi:schemaLocation='http://www.pcraster.nl/pcrxml commonTypes.xsd'>";
  o << duration << "</pcr:unitTestOnlyDuration> ";
  std::istringstream i(o.str());
  std::auto_ptr<pcrxml::TimeDuration> d(pcrxml::unitTestOnlyDuration(i));
  return *d;
}

//------------------------------------------------------------------------------
// DEFINITION OF COMMONTYPES MEMBERS
//------------------------------------------------------------------------------

//! ctor
CommonTypesTest::CommonTypesTest(
         )
{
}



//! setUp
void CommonTypesTest::setUp()
{
}



//! tearDown
void CommonTypesTest::tearDown()
{
}



void CommonTypesTest::testToBoostPosixTime()
{
  {
  boost::posix_time::ptime p1= toPosixTime(
      makeDateTime("2005-02-10T18:15:00"));
  BOOST_CHECK(p1.date().year()==2005);
  BOOST_CHECK(p1.date().month()==2);
  BOOST_CHECK(p1.date().day()==10);
  BOOST_CHECK(p1.time_of_day().hours()==18);
  BOOST_CHECK(p1.time_of_day().minutes()==15);
  BOOST_CHECK(p1.time_of_day().seconds()==0);
  BOOST_CHECK(p1.time_of_day().fractional_seconds()== 0);
  }

  {
  boost::posix_time::ptime p1= toPosixTime(makeDateTime("2005-02-10T18:15:00.345"));
  BOOST_CHECK(p1.date().year()==2005);
  BOOST_CHECK(p1.date().month()==2);
  BOOST_CHECK(p1.date().day()==10);
  BOOST_CHECK(p1.time_of_day().hours()==18);
  BOOST_CHECK(p1.time_of_day().minutes()==15);
  BOOST_CHECK(p1.time_of_day().seconds()==0);
  // fractional_seconds not supported:
  // BOOST_CHECK_EQUAL(p1.time_of_day().fractional_seconds(), 345000);
  }


  // timezones not supported:
  // toPosixTime(makeDateTime("-2005-02-10T18:15:00.345"));
  // toPosixTime(makeDateTime("2005-02-10T18:15:00.345+02:00"));
  // toPosixTime(makeDateTime("-2005-02-10T18:15:00.345+02:00"));
  // toPosixTime(makeDateTime("2005-02-10T18:15:00.345-02:00"));
  // toPosixTime(makeDateTime("-2005-02-10T18:15:00.345-02:00"));
  // toPosixTime(makeDateTime("-2005-02-10T18:15:00.345Z"));
  // toPosixTime(makeDateTime("2005-02-10T18:15:00.345Z"));
}

void CommonTypesTest::testTimeDurationAssumption()
{
    using namespace boost::posix_time;
    namespace bg = boost::gregorian;

 {
    // test the assumption that more then 24 hours is OK
    ptime time;
    time = ptime(bg::date(2006, boost::gregorian::Feb, 1),
                 time_duration(0, 0, 0, 0));
    time_duration duration(4098, 0, 0, 0);
    time += duration;

    BOOST_CHECK(time.date().year()==2006);
    BOOST_CHECK(time.date().month()==7);
    BOOST_CHECK(time.date().day()==21);
    BOOST_CHECK(time.time_of_day().hours()==18);
    BOOST_CHECK(time.time_of_day().minutes()==0);
    BOOST_CHECK(time.time_of_day().seconds()==0);
    BOOST_CHECK(time.time_of_day().fractional_seconds()== 0);
 }
 {
    // can expess the same in minutes
    ptime time = ptime(bg::date(2006, boost::gregorian::Feb, 1),
                 time_duration(0, 0, 0, 0));
    time_duration duration(0,4098*60, 0, 0);
    time += duration;

    BOOST_CHECK(time.date().year()==2006);
    BOOST_CHECK(time.date().month()==7);
    BOOST_CHECK(time.date().day()==21);
    BOOST_CHECK(time.time_of_day().hours()==18);
    BOOST_CHECK(time.time_of_day().minutes()==0);
    BOOST_CHECK(time.time_of_day().seconds()==0);
    BOOST_CHECK(time.time_of_day().fractional_seconds()== 0);
  }
}

void CommonTypesTest::testTimeDuration()
{
  {
  boost::posix_time::time_duration p =
    toPosixTimeDuration(makeTimeDuration("<pcr:hours>10</pcr:hours>"));
  BOOST_CHECK(p.hours()==10);
  }

  {
  boost::posix_time::time_duration p =
    toPosixTimeDuration(makeTimeDuration("<pcr:minutes>10</pcr:minutes>"));
  BOOST_CHECK(p.minutes()==10);
  }
  {
  boost::posix_time::time_duration p =
    toPosixTimeDuration(makeTimeDuration("<pcr:minutes>100</pcr:minutes>"));
  BOOST_CHECK(p.hours()==1);
  BOOST_CHECK(p.minutes()==40);
  }
  {
  boost::posix_time::time_duration p =
    toPosixTimeDuration(makeTimeDuration("<pcr:seconds>100</pcr:seconds>"));
  BOOST_CHECK(p.minutes()==1);
  BOOST_CHECK(p.seconds()==40);
  }
}

} // namespace pcrxsd

