#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_GRIDDEDPOINTSTEST
#include "geo_griddedpointstest.h"
#define INCLUDED_GEO_GRIDDEDPOINTSTEST
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
#ifndef INCLUDED_GEO_GRIDDEDPOINTS
#include "geo_griddedpoints.h"
#define INCLUDED_GEO_GRIDDEDPOINTS
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif

/*!
  \file
  This file contains the implementation of the GriddedPointsTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC GRIDDEDPOINTS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::GriddedPointsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<GriddedPointsTest> instance(new GriddedPointsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&GriddedPointsTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&GriddedPointsTest::testNrPoints, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&GriddedPointsTest::testPoints, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&GriddedPointsTest::testCopy, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF GRIDDEDPOINTS MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::GriddedPointsTest::GriddedPointsTest()
{
}



//! setUp
void geo::GriddedPointsTest::setUp()
{
}

//! tearDown
void geo::GriddedPointsTest::tearDown()
{
}




void geo::GriddedPointsTest::testConstructor()
{
  RasterSpace space(5, 6, 10.0, 0.0, 1.0, YIncrB2T);
  GriddedPoints<Point<double, 2> > points(space);
  // BOOST_CHECK(false);
}



void geo::GriddedPointsTest::testNrPoints()
{
  // Raster with one row and two columns.
  RasterSpace space(1, 2, 1.0, 0.0, 0.0, YIncrB2T);
  GriddedPoints<Point<double, 2> > points(space);

  BOOST_CHECK(points.nrPoints(0, 0) == 0);
  BOOST_CHECK(points.nrPoints(0, 1) == 0);

  // First cell is MV. Second not.
  points.setMV(0, 0);

  Point<double, 2> p;
  p[0] = 1.0;
  p[1] = 0.0;

  points.insert(p);
  points.insert(p);
  points.insert(p);

  BOOST_CHECK(points.nrPoints(0, 0) == 0);
  BOOST_CHECK(points.nrPoints(0, 1) == 3);
}



void geo::GriddedPointsTest::testPoints()
{
  std::vector<Point<double, 2> > points;

  {
    RasterSpace space(1, 1, 1.0, 0.0, 0.0, YIncrB2T);
    GriddedPoints<Point<double, 2> > griddedPoints(space);

    points.clear();
    griddedPoints.points(CellLoc(0, 0), 3, points);
    BOOST_CHECK(points.empty());

    griddedPoints.insert(Point<double, 2>(0.0, 0.0));
    points.clear();
    griddedPoints.points(CellLoc(0, 0), 1, points);
    BOOST_CHECK(points.size() == 1);

    points.clear();
    griddedPoints.points(CellLoc(0, 0), 3, points);
    BOOST_CHECK(points.size() == 1);

    griddedPoints.insert(Point<double, 2>(0.0, 0.0));
    points.clear();
    griddedPoints.points(CellLoc(0, 0), 1, points);
    BOOST_CHECK(points.size() == 2);

    points.clear();
    griddedPoints.points(CellLoc(0, 0), 3, points);
    BOOST_CHECK(points.size() == 2);
  }

  {
    RasterSpace space(3, 3, 1.0, 0.0, 0.0, YIncrT2B);
    GriddedPoints<Point<double, 2> > griddedPoints(space);

    points.clear();
    griddedPoints.points(CellLoc(0, 0), 1, points);
    BOOST_CHECK(points.empty());

    // Put a point in each cell.
    for(size_t row = 0; row < griddedPoints.nrRows(); ++row) {
      for(size_t col = 0; col < griddedPoints.nrCols(); ++col) {
        griddedPoints.insert(
              Point<double, 2>(row * 1.0, col * 1.0));
      }
    }

    // 5 points in a circular neighbourhood with radius 1.
    points.clear();
    griddedPoints.points(CellLoc(1, 1), 1, points);
    BOOST_CHECK(points.size() == 5);
  }
}



void geo::GriddedPointsTest::testCopy()
{
  // Create and fill an object.
    RasterSpace space(3, 3, 1.0, 0.0, 0.0, YIncrT2B);
    GriddedPoints<Point<double, 2> > source(space);
    for(size_t row = 0; row < source.nrRows(); ++row) {
      for(size_t col = 0; col < source.nrCols(); ++col) {
        source.insert(Point<double, 2>(row * 1.0, col * 1.0));
      }
    }

  // Create a copy.
    GriddedPoints<Point<double, 2> > target(source);

  // Compare source and target objects.
    BOOST_CHECK(source.size() == target.size());

}


