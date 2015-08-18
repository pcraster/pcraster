#include "com_rangemaptest.h"

// Library headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// PCRaster library headers.
#include "com_rangemap.h"



/*!
  \file
  This file contains the implementation of the RangeMapTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RANGEMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::RangeMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RangeMapTest> instance(new RangeMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RangeMapTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RANGEMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::RangeMapTest::RangeMapTest()
{
}



//! setUp
void com::RangeMapTest::setUp()
{
}

//! tearDown
void com::RangeMapTest::tearDown()
{
}



void com::RangeMapTest::test()
{
  RangeMap<double, double> map(1.0, 2.0, 0.0, 100.0);
  BOOST_CHECK(map.map(1.5) == 50.0);

  map.setRanges(0.0, 0.0, 3.0, 100.0);
  BOOST_CHECK(map.map(1.5) == 3.0);

  map.setRanges(1.0, 2.0, 0.0, 0.0);
  BOOST_CHECK(map.map(1.5) == 0.0);
}

