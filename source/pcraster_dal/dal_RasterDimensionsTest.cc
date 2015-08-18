#ifndef INCLUDED_DAL_RASTERDIMENSIONSTEST
#include "dal_RasterDimensionsTest.h"
#define INCLUDED_DAL_RASTERDIMENSIONSTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



/*!
  \file
  This file contains the implementation of the RasterDimensionsTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDIMENSIONSTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* RasterDimensionsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterDimensionsTest> instance(new RasterDimensionsTest());
  suite->add(BOOST_CLASS_TEST_CASE(&RasterDimensionsTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDimensionsTest::testAssignment, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDimensionsTest::testIndex, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDimensionsTest::testCoordinates, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDimensionsTest::testAreaDimensions, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDimensionsTest::testOverlap, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDIMENSIONSTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
RasterDimensionsTest::RasterDimensionsTest()
{
}



void RasterDimensionsTest::test()
{
  // Default.
  {
    RasterDimensions raster;

    BOOST_CHECK(dal::comparable(raster.cellSize(), 1.0));
    BOOST_CHECK(dal::comparable(raster.west(), 0.0));
    BOOST_CHECK(dal::comparable(raster.north(), 0.0));
    BOOST_CHECK(dal::comparable(raster.east(), 1.0));
    BOOST_CHECK(dal::comparable(raster.south(), -1.0));
  }

  // Non-default.
  {
    RasterDimensions raster(3, 4);

    BOOST_CHECK(dal::comparable(raster.cellSize(), 1.0));
    BOOST_CHECK(dal::comparable(raster.west(), 0.0));
    BOOST_CHECK(dal::comparable(raster.north(), 0.0));
    BOOST_CHECK(dal::comparable(raster.east(), 4.0));
    BOOST_CHECK(dal::comparable(raster.south(), -3.0));
  }

  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);

    BOOST_CHECK(dal::comparable(raster.cellSize(), 5.0));
    BOOST_CHECK(dal::comparable(raster.west(), 1.0));
    BOOST_CHECK(dal::comparable(raster.north(), 2.0));
    BOOST_CHECK(dal::comparable(raster.east(), 21.0));
    BOOST_CHECK(dal::comparable(raster.south(), -13.0));
  }

  // Copy.
  {
    RasterDimensions
         raster1(3, 4, 5.0, 1.0, 2.0),
         raster2(3, 4, 5.0, 1.0, 3.0),
         raster3(3, 4, 5.0, 1.0, 2.0),
         raster4(3, 5, 5.0, 1.0, 2.0);

    BOOST_CHECK(raster1 == raster1);
    BOOST_CHECK(raster1 != raster2);
    BOOST_CHECK(raster1 == raster3);
    BOOST_CHECK(raster1 != raster4);
  }
}



void RasterDimensionsTest::testAssignment()
{
  {
    RasterDimensions raster1(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions raster2(3, 5, 5.0, 1.0, 2.0);
    RasterDimensions raster;

    raster = raster1;
    BOOST_CHECK(raster == raster1);
    BOOST_CHECK(raster != raster2);

    raster = raster2;
    BOOST_CHECK(raster != raster1);
    BOOST_CHECK(raster == raster2);
  }
}



void RasterDimensionsTest::testIndex()
{
  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);

    BOOST_CHECK_EQUAL(raster.index(size_t(0), size_t(0)), size_t(0));
    BOOST_CHECK_EQUAL(raster.index(size_t(0), size_t(1)), size_t(1));
    BOOST_CHECK_EQUAL(raster.index(size_t(1), size_t(0)), raster.nrCols());

    BOOST_CHECK_EQUAL(raster.index(1.0, 2.0), size_t(0));
    BOOST_CHECK_EQUAL(raster.index(20.9, 2.0), size_t(3));
    BOOST_CHECK_EQUAL(raster.index(1.0, -12.9), size_t(8));
    BOOST_CHECK_EQUAL(raster.index(20.9, -12.9), size_t(11));
  }
}



void RasterDimensionsTest::testCoordinates()
{
  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);
    double x, y;

    raster.coordinates(0u, x, y);
    BOOST_CHECK_CLOSE(x, 3.5, 0.001);
    BOOST_CHECK_CLOSE(y, -0.5, 0.001);

    raster.coordinates(11u, x, y);
    BOOST_CHECK_CLOSE(x, 18.5, 0.001);
    BOOST_CHECK_CLOSE(y, -10.5, 0.001);
  }
}



void RasterDimensionsTest::testAreaDimensions()
{
  // Empty area.
  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions area(raster.areaDimensions(1.0, 2.0, 1.0, 2.0));
    BOOST_CHECK_EQUAL(area.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area.cellSize(), raster.cellSize());
    BOOST_CHECK_EQUAL(area.west(), raster.west());
    BOOST_CHECK_EQUAL(area.north(), raster.north());
  }

  // Small area.
  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions area(raster.areaDimensions(1.0, 2.0, 1.1, 1.9));
    BOOST_CHECK_EQUAL(area.nrRows(), size_t(1));
    BOOST_CHECK_EQUAL(area.nrCols(), size_t(1));
    BOOST_CHECK_EQUAL(area.cellSize(), raster.cellSize());
    BOOST_CHECK_EQUAL(area.west(), raster.west());
    BOOST_CHECK_EQUAL(area.north(), raster.north());
  }

  // Area on cell border.
  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions area(raster.areaDimensions(1.0, 2.0, 6.0, -3.0));
    BOOST_CHECK_EQUAL(area.nrRows(), size_t(1));
    BOOST_CHECK_EQUAL(area.nrCols(), size_t(1));
    BOOST_CHECK_EQUAL(area.cellSize(), raster.cellSize());
    BOOST_CHECK_EQUAL(area.west(), raster.west());
    BOOST_CHECK_EQUAL(area.north(), raster.north());
  }

  // Area just over cell border.
  {
    RasterDimensions raster(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions area(raster.areaDimensions(1.0, 2.0, 6.1, -3.1));
    BOOST_CHECK_EQUAL(area.nrRows(), size_t(2));
    BOOST_CHECK_EQUAL(area.nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(area.cellSize(), raster.cellSize());
    BOOST_CHECK_EQUAL(area.west(), raster.west());
    BOOST_CHECK_EQUAL(area.north(), raster.north());
  }
}



void RasterDimensionsTest::testOverlap()
{
  RasterDimensions area1, area2;

  // Empty raster dimensions.
  {
    RasterDimensions raster(size_t(0), size_t(0));

    boost::tie(area1, area2) = RasterDimensions::overlap(raster, raster);
    BOOST_CHECK_EQUAL(area1.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area1.nrCols(), size_t(0));
    BOOST_CHECK(area1 == area2);
  }

  // One empty, one non-empty raster dimension.
  {
    RasterDimensions raster1(size_t(0), size_t(0));
    RasterDimensions raster2(3, 4, 5.0, 1.0, 2.0);

    boost::tie(area1, area2) = RasterDimensions::overlap(raster1, raster2);
    BOOST_CHECK_EQUAL(area1.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area1.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area1.cellSize(), raster1.cellSize());
    BOOST_CHECK_EQUAL(area1.west(), raster1.west());
    BOOST_CHECK_EQUAL(area1.north(), raster1.north());
    BOOST_CHECK_EQUAL(area2.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area2.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area2.cellSize(), raster2.cellSize());
    BOOST_CHECK_EQUAL(area2.west(), raster2.west());
    BOOST_CHECK_EQUAL(area2.north(), raster2.north());

    // Switch arguments.
    boost::tie(area2, area1) = RasterDimensions::overlap(raster2, raster1);
    BOOST_CHECK_EQUAL(area2.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area2.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area2.cellSize(), raster2.cellSize());
    BOOST_CHECK_EQUAL(area2.west(), raster2.west());
    BOOST_CHECK_EQUAL(area2.north(), raster2.north());
    BOOST_CHECK_EQUAL(area1.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area1.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area1.cellSize(), raster1.cellSize());
    BOOST_CHECK_EQUAL(area1.west(), raster1.west());
    BOOST_CHECK_EQUAL(area1.north(), raster1.north());
  }

  // Non-overlapping raster dimensions.
  {
    RasterDimensions raster1(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions raster2(3, 4, 5.0, 22, -14.0);

    boost::tie(area1, area2) = RasterDimensions::overlap(raster1, raster2);
    BOOST_CHECK_EQUAL(area1.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area1.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area1.cellSize(), raster1.cellSize());
    BOOST_CHECK_EQUAL(area1.west(), raster1.west());
    BOOST_CHECK_EQUAL(area1.north(), raster1.north());
    BOOST_CHECK_EQUAL(area2.nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(area2.nrCols(), size_t(0));
    BOOST_CHECK_EQUAL(area2.cellSize(), raster2.cellSize());
    BOOST_CHECK_EQUAL(area2.west(), raster2.west());
    BOOST_CHECK_EQUAL(area2.north(), raster2.north());
  }

  // Equal raster dimensions.
  {
    RasterDimensions raster1(3, 4, 5.0, 1.0, 2.0);
    RasterDimensions raster2(3, 4, 5.0, 1.0, 2.0);

    assert(raster1 == raster2);
    boost::tie(area1, area2) = RasterDimensions::overlap(raster1, raster2);
    BOOST_CHECK(area1 == raster1);
    BOOST_CHECK(area2 == raster2);
  }

  // Overlapping raster dimensions.
  {
    {
      // Second raster is offset to south-east by one row and column.
      RasterDimensions raster1(3, 4, 5.0, 1.0, 2.0);
      RasterDimensions raster2(3, 4, 5.0, 6.0, -3.0);

      boost::tie(area1, area2) = RasterDimensions::overlap(raster1, raster2);
      BOOST_CHECK_EQUAL(area1.nrRows(), size_t(2));
      BOOST_CHECK_EQUAL(area1.nrCols(), size_t(3));
      BOOST_CHECK_EQUAL(area1.cellSize(), raster1.cellSize());
      BOOST_CHECK_EQUAL(area1.west(), 6.0);
      BOOST_CHECK_EQUAL(area1.north(), -3.0);
      BOOST_CHECK(area2 == area1);
    }

    {
      // Second raster is offset to north-west by one row and column.
      RasterDimensions raster1(3, 4, 5.0, 1.0, 2.0);
      RasterDimensions raster2(3, 4, 5.0, -4.0, 7.0);

      boost::tie(area1, area2) = RasterDimensions::overlap(raster1, raster2);
      BOOST_CHECK_EQUAL(area1.nrRows(), size_t(2));
      BOOST_CHECK_EQUAL(area1.nrCols(), size_t(3));
      BOOST_CHECK_EQUAL(area1.cellSize(), raster1.cellSize());
      BOOST_CHECK_EQUAL(area1.west(), 1.0);
      BOOST_CHECK_EQUAL(area1.north(), 2.0);
      BOOST_CHECK(area2 == area1);
    }
  }
}

} // namespace dal

