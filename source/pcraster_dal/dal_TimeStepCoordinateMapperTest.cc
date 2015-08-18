#ifndef INCLUDED_DAL_TIMESTEPCOORDINATEMAPPERTEST
#include "dal_TimeStepCoordinateMapperTest.h"
#define INCLUDED_DAL_TIMESTEPCOORDINATEMAPPERTEST
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

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the TimeStepCoordinateMapperTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMESTEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*TimeStepCoordinateMapperTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TimeStepCoordinateMapperTest> instance(new TimeStepCoordinateMapperTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TimeStepCoordinateMapperTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TIMESTEPCOORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

//! ctor
TimeStepCoordinateMapperTest::TimeStepCoordinateMapperTest(
         )
{
}



//! setUp
void TimeStepCoordinateMapperTest::setUp()
{
}



//! tearDown
void TimeStepCoordinateMapperTest::tearDown()
{
}



void TimeStepCoordinateMapperTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



} // namespace dal

