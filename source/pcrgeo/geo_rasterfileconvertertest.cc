#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTERFILECONVERTERTEST
#include "geo_rasterfileconvertertest.h"
#define INCLUDED_GEO_RASTERFILECONVERTERTEST
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
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.
#ifndef INCLUDED_GEO_RASTERFILECONVERTER
#include "geo_rasterfileconverter.h"
#define INCLUDED_GEO_RASTERFILECONVERTER
#endif



/*!
  \file
  This file contains the implementation of the RasterFileConverterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERFILECONVERTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::RasterFileConverterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterFileConverterTest> instance(new RasterFileConverterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RasterFileConverterTest::testBil2Ascii, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTERFILECONVERTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::RasterFileConverterTest::RasterFileConverterTest()
{
}



//! setUp
void geo::RasterFileConverterTest::setUp()
{
}

//! tearDown
void geo::RasterFileConverterTest::tearDown()
{
}



void geo::RasterFileConverterTest::testBil2Ascii()
{
  RasterFileConverter b2a("all1_float.bil");
  b2a.writeAscii("bil2ascii.txt");
}
