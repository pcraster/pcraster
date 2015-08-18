#include "com_utiltest.h"
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>
#include "com_util.h"



/*!
  \file
  This file contains the implementation of the UtilTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::UtilTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<UtilTest> instance(new UtilTest());

  suite->add(BOOST_CLASS_TEST_CASE(&UtilTest::testSmallestDivisor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&UtilTest::testLargestDivisor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::UtilTest::UtilTest()
{
}



void com::UtilTest::setUp()
{
}



void com::UtilTest::tearDown()
{
}



void com::UtilTest::testSmallestDivisor()
{
  BOOST_CHECK(smallestDivisor(10, 1) == 1);
  BOOST_CHECK(smallestDivisor(10, 2) == 2);
  BOOST_CHECK(smallestDivisor(10, 3) == 5);
  BOOST_CHECK(smallestDivisor(10, 4) == 5);
  BOOST_CHECK(smallestDivisor(10, 6) == 10);
  BOOST_CHECK(smallestDivisor(10, 10) == 10);
}



void com::UtilTest::testLargestDivisor()
{
  BOOST_CHECK(largestDivisor(10, 10) == 10);
  BOOST_CHECK(largestDivisor(10, 9) == 5);
  BOOST_CHECK(largestDivisor(10, 6) == 5);
  BOOST_CHECK(largestDivisor(10, 5) == 5);
  BOOST_CHECK(largestDivisor(10, 4) == 2);
  BOOST_CHECK(largestDivisor(10, 2) == 2);
  BOOST_CHECK(largestDivisor(10, 1) == 1);
}
