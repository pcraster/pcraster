#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_RASTER_RASTERTEST
#include "raster_rastertest.h"
#define INCLUDED_RASTER_RASTERTEST
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
  This file contains the implementation of the RasterTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace raster {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*RasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterTest> instance(new RasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RasterTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterTest::testSetMV, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
RasterTest::RasterTest(
         )
{
}



//! setUp
void RasterTest::setUp()
{
}



//! tearDown
void RasterTest::tearDown()
{
}



void RasterTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



void RasterTest::testSetMV()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace raster

