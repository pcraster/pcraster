#define BOOST_TEST_MODULE pcraster geo algorithm
#include <boost/test/unit_test.hpp>
#include <iterator>
#include "geo_algorithm.h"
#include "geo_point.h"
#include "geo_pointvalue.h"


BOOST_AUTO_TEST_CASE(points_in_area)
{
  using namespace geo;

  {
    typedef Point<int, 2> Punt;
    typedef double Value;
    typedef std::vector<PointValue<Punt, Value> > PointValues;

    Value value(0.0);
    PointValues points;
    size_t nrRows = 3;
    size_t nrCols = 3;

    for(size_t row = 0; row < nrRows; ++row) {
      for(size_t col = 0; col < nrCols; ++col) {
        points.push_back(PointValue<Punt, Value>(Punt(row, col), value));
      }
    }

    Punt point(1, 1);
    PointValues subset;
    Punt::CoordinateType radius = 0;

    subset.clear();
    pointsInArea(point, radius, points.begin(), points.end(),
         std::back_inserter(subset));
    BOOST_CHECK(subset.size() == 9);

    radius = 1;
    subset.clear();
    pointsInArea(point, radius, points.begin(), points.end(),
         std::back_inserter(subset));
    BOOST_CHECK(subset.size() == 5);

    radius = 2;
    subset.clear();
    pointsInArea(point, radius, points.begin(), points.end(),
         std::back_inserter(subset));
    BOOST_CHECK(subset.size() == 9);
  }
}


BOOST_AUTO_TEST_CASE(maximum_)
{
  using namespace geo;

  {
    typedef Point<int, 2> Punt;
    typedef int Value;
    typedef std::vector<PointValue<Punt, Value> > PointValues;

    PointValues points;
    size_t nrRows = 3;
    size_t nrCols = 3;

    for(size_t row = 0; row < nrRows; ++row) {
      for(size_t col = 0; col < nrCols; ++col) {
        points.push_back(PointValue<Punt, Value>(Punt(row, col),
              row * nrCols + col));
      }
    }

    Punt point(1, 1);
    PointValues subset;
    Punt::CoordinateType radius;
    bool result;
    Value max;

    radius = 0;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);

    radius = 1;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 7);

    radius = 2;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);
  }

  {
    typedef Point<double, 2> Point;
    typedef int Value;
    typedef std::vector<PointValue<Point, Value> > PointValues;

    PointValues points;
    size_t nrRows = 3;
    size_t nrCols = 3;

    for(size_t row = 0; row < nrRows; ++row) {
      for(size_t col = 0; col < nrCols; ++col) {
        points.push_back(PointValue<Point, Value>(Point(row, col),
              row * nrCols + col));
      }
    }

    Point point(1, 1);
    PointValues subset;
    Point::CoordinateType radius;
    bool result;
    Value max;

    radius = 0;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);

    radius = 1;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 7);

    radius = 2;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);
  }
}
