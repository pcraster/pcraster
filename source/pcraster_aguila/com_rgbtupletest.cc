#include "com_rgbtupletest.h"

// Library headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// PCRaster library headers.

// Module headers.
#include "com_rgbtuple.h"



/*!
  \file
  This file contains the implementation of the RgbTupleTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RGBTUPLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::RgbTupleTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RgbTupleTest> instance(new RgbTupleTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RgbTupleTest::testEq, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RGBTUPLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::RgbTupleTest::RgbTupleTest()
{
}



//! setUp
void com::RgbTupleTest::setUp()
{
}

//! tearDown
void com::RgbTupleTest::tearDown()
{
}



void com::RgbTupleTest::testEq()
{
  RgbTuple gray25(64,64,64);
  BOOST_CHECK(gray25 == gray25);
  BOOST_CHECK(gray25 != RgbTuple::gray50_);
  BOOST_CHECK(RgbTuple::gray50_ == RgbTuple::gray50_);
  BOOST_CHECK(RgbTuple::gray50_ != RgbTuple::black_);
}
