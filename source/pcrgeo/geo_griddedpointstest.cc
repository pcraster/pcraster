#define BOOST_TEST_MODULE pcraster geo gridded_points
#include <boost/test/unit_test.hpp>
#include "geo_griddedpoints.h"
#include "geo_rasterspace.h"
#include "geo_point.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace geo;

  RasterSpace space(5, 6, 10.0, 0.0, 1.0, YIncrB2T);
  GriddedPoints<Point<double, 2> > points(space);
  // BOOST_CHECK(false);
}


BOOST_AUTO_TEST_CASE(nr_points)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(points)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(copy)
{
  using namespace geo;

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
