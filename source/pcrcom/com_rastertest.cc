#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_RASTERTEST
#include "com_rastertest.h"
#define INCLUDED_COM_RASTERTEST
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
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
// Module headers.
#ifndef INCLUDED_COM_RASTER
#include "com_raster.h"
#define INCLUDED_COM_RASTER
#endif



/*!
  \file
  This file contains the implementation of the RasterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::RasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterTest> instance(new RasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RasterTest::testConstructorSingleValue, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::RasterTest::RasterTest()
{
}



//! setUp
void com::RasterTest::setUp()
{
}

//! tearDown
void com::RasterTest::tearDown()
{
}



void com::RasterTest::testConstructorSingleValue()
{
  Raster<UINT1> v(4,5,8);
  BOOST_CHECK(v.nrRows()==4);
  BOOST_CHECK(v.nrCols()==5);
  BOOST_CHECK(v.cell(0, 0)==8);
  BOOST_CHECK(v.cell(3,4)==8);

  v.setMV(0,2);
  BOOST_CHECK(v.isMV(0,2));
  BOOST_CHECK(!v.isMV(3,4));
}
