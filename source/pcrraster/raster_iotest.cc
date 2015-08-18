#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_RASTER_IOTEST
#include "raster_iotest.h"
#define INCLUDED_RASTER_IOTEST
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
  This file contains the implementation of the IOTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace raster {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC IO MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*IOTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<IOTest> instance(new IOTest());

  suite->add(BOOST_CLASS_TEST_CASE(&IOTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF IO MEMBERS
//------------------------------------------------------------------------------

//! ctor
IOTest::IOTest(
         )
{
}



//! setUp
void IOTest::setUp()
{
}



//! tearDown
void IOTest::tearDown()
{
}



void IOTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



} // namespace raster

