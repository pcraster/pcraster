#define BOOST_TEST_MODULE pcraster dal time_step_mapper
#include <boost/test/unit_test.hpp>
#include "dal_TimeStepMapper.h"
#include "dal_MathUtils.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

  // Dataset A:
  // 1 - 28
  // 20060201 - 20060228 (days)
  //
  // Dataset B:
  // 1 - 365
  // 20060101 - 20061231 (days)
  //
  // Common dimension:
  // 20060101 - 20061231 (days)

  namespace bp = boost::posix_time;
  namespace bg = boost::gregorian;

  bp::ptime time;

  time = bp::ptime(bg::date(2006, boost::gregorian::Feb, 1),
         bp::time_duration(0, 0, 0, 0));
  bp::time_duration duration(24, 0, 0, 0);

  TimeStepMapper mapperA(1.0, time, duration);
  BOOST_CHECK(mapperA.destination(1.0) == time);
  BOOST_CHECK(mapperA.destination(2.0) == time + bg::days(1));
  BOOST_CHECK(comparable(mapperA.source(time), 1.0));
  BOOST_CHECK(comparable(mapperA.source(time + bg::days(1)), 2.0));
  BOOST_CHECK(comparable(mapperA.source(time - bg::days(2)), -1.0));

  time = bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
         bp::time_duration(0, 0, 0, 0));

  TimeStepMapper mapperB(1.0, time, duration);
  BOOST_CHECK(comparable(mapperB.source(bp::ptime(
         bg::date(2006, boost::gregorian::Dec, 31),
         bp::time_duration(0, 0, 0, 0))), 365.0));

  TimeStepMapper mapper(mapperA);
  mapper |= mapperB;

  BOOST_CHECK(comparable(mapper.source(bp::ptime(
         bg::date(2006, boost::gregorian::Jan, 1),
         bp::time_duration(0, 0, 0, 0))), 1.0));
  BOOST_CHECK(comparable(mapper.source(bp::ptime(
         bg::date(2006, boost::gregorian::Feb, 1),
         bp::time_duration(0, 0, 0, 0))), 32.0));
  BOOST_CHECK(comparable(mapper.source(bp::ptime(
         bg::date(2006, boost::gregorian::Dec, 31),
         bp::time_duration(0, 0, 0, 0))), 365.0));
}
