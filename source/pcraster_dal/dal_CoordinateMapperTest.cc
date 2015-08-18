#ifndef INCLUDED_DAL_COORDINATEMAPPERTEST
#include "dal_CoordinateMapperTest.h"
#define INCLUDED_DAL_COORDINATEMAPPERTEST
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
  This file contains the implementation of the CoordinateMapperTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*CoordinateMapperTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CoordinateMapperTest> instance(new CoordinateMapperTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CoordinateMapperTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COORDINATEMAPPER MEMBERS
//------------------------------------------------------------------------------

//! ctor
CoordinateMapperTest::CoordinateMapperTest(
         )
{
}



//! setUp
void CoordinateMapperTest::setUp()
{
}



//! tearDown
void CoordinateMapperTest::tearDown()
{
}



void CoordinateMapperTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



} // namespace dal

