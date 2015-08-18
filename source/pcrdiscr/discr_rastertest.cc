#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_RASTERTEST
#include "discr_rastertest.h"
#define INCLUDED_DISCR_RASTERTEST
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
#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the RasterTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*RasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterTest> instance(new RasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RasterTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterTest::testEquals, instance));

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



void RasterTest::testConstructor()
{
  size_t nrRows = 3;
  size_t nrCols = 4;
  double cellSize = 1.5;
  double west = 1.0;
  double north = 0.0;

  {
    Raster raster(nrRows, nrCols);
    BOOST_CHECK(raster.nrRows() == nrRows);
    BOOST_CHECK(raster.nrCols() == nrCols);
    BOOST_CHECK(raster.nrCells() == nrRows * nrCols);
    BOOST_CHECK(raster.cellSize() == 1.0);
    BOOST_CHECK(raster.west() == 0.0);
    BOOST_CHECK(raster.north() == 0.0);
  }

  {
    Raster raster(nrRows, nrCols, cellSize, west, north);
    BOOST_CHECK(raster.nrRows() == nrRows);
    BOOST_CHECK(raster.nrCols() == nrCols);
    BOOST_CHECK(raster.nrCells() == nrRows * nrCols);
    BOOST_CHECK(raster.cellSize() == cellSize);
    BOOST_CHECK(raster.west() == west);
    BOOST_CHECK(raster.north() == north);
  }
}



void RasterTest::testEquals()
{
  Raster raster1(3, 4, 1.5, 1.0, 0.0);
  Raster raster2(3, 4, 1.5, 1.0, 1.0);
  BOOST_CHECK(raster1 == raster1);
  BOOST_CHECK(raster2 == raster2);
  BOOST_CHECK(raster1 != raster2);
  BOOST_CHECK(raster2 != raster1);
}

} // namespace discr

