#define BOOST_TEST_MODULE pcraster geo scan_conversion
#include <boost/test/unit_test.hpp>
#include "geo_scanconversion.h"
#include <algorithm>

BOOST_AUTO_TEST_CASE(midpoint_line)
{
  using namespace geo;

  {
    // N line.
    RememberPoints<size_t> points;
    points = midpointLine(0, 0, 0, 2, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(0, 0)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(0, 1)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(0, 2)));
  }

  {
    // S line.
    RememberPoints<size_t> points;
    points = midpointLine(0, 2, 0, 0, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(0, 2)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(0, 1)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(0, 0)));
  }

  {
    // E line.
    RememberPoints<size_t> points;
    points = midpointLine(0, 0, 2, 0, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(0, 0)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(1, 0)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(2, 0)));
  }

  {
    // W line.
    RememberPoints<size_t> points;
    points = midpointLine(2, 0, 0, 0, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(2, 0)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(1, 0)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(0, 0)));
  }

  {
    // NE line.
    RememberPoints<size_t> points;
    points = midpointLine(0, 0, 2, 2, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(0, 0)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(1, 1)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(2, 2)));
  }

  {
    // NW line.
    RememberPoints<size_t> points;
    points = midpointLine(2, 0, 0, 2, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(2, 0)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(1, 1)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(0, 2)));
  }

  {
    // SW line.
    RememberPoints<size_t> points;
    points = midpointLine(2, 2, 0, 0, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(2, 2)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(1, 1)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(0, 0)));
  }

  {
    // SE line.
    RememberPoints<size_t> points;
    points = midpointLine(0, 2, 2, 0, points);
    BOOST_CHECK(points.size() == 3);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(0, 2)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(1, 1)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(2, 0)));
  }

  {
    // ENE line.
    RememberPoints<size_t> points;
    points = midpointLine(5, 8, 9, 11, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(5, 8)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(6, 9)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(7, 9)));
    BOOST_CHECK(points[3] == (std::pair<size_t, size_t>(8, 10)));
    BOOST_CHECK(points[4] == (std::pair<size_t, size_t>(9, 11)));
  }

  {
    // WSW line.
    RememberPoints<int> points;
    points = midpointLine(-5, -8, -9, -11, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<int, int>(-5, -8)));
    BOOST_CHECK(points[1] == (std::pair<int, int>(-6, -9)));
    BOOST_CHECK(points[2] == (std::pair<int, int>(-7, -9)));
    BOOST_CHECK(points[3] == (std::pair<int, int>(-8, -10)));
    BOOST_CHECK(points[4] == (std::pair<int, int>(-9, -11)));
  }

  {
    // NNE line.
    RememberPoints<size_t> points;
    points = midpointLine(8, 5, 11, 9, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<size_t, size_t>(8, 5)));
    BOOST_CHECK(points[1] == (std::pair<size_t, size_t>(9, 6)));
    BOOST_CHECK(points[2] == (std::pair<size_t, size_t>(9, 7)));
    BOOST_CHECK(points[3] == (std::pair<size_t, size_t>(10, 8)));
    BOOST_CHECK(points[4] == (std::pair<size_t, size_t>(11, 9)));
  }

  {
    // ESE line.
    RememberPoints<int> points;
    points = midpointLine<int>(5, -8, 9, -11, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<int, int>(5, -8)));
    BOOST_CHECK(points[1] == (std::pair<int, int>(6, -9)));
    BOOST_CHECK(points[2] == (std::pair<int, int>(7, -9)));
    BOOST_CHECK(points[3] == (std::pair<int, int>(8, -10)));
    BOOST_CHECK(points[4] == (std::pair<int, int>(9, -11)));
  }

  {
    // WNW line.
    RememberPoints<int> points;
    points = midpointLine(-5, 8, -9, 11, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<int, int>(-5, 8)));
    BOOST_CHECK(points[1] == (std::pair<int, int>(-6, 9)));
    BOOST_CHECK(points[2] == (std::pair<int, int>(-7, 9)));
    BOOST_CHECK(points[3] == (std::pair<int, int>(-8, 10)));
    BOOST_CHECK(points[4] == (std::pair<int, int>(-9, 11)));
  }

  {
    // SSE line.
    RememberPoints<int> points;
    points = midpointLine<int>(8, -5, 11, -9, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<int, int>(8, -5)));
    BOOST_CHECK(points[1] == (std::pair<int, int>(9, -6)));
    BOOST_CHECK(points[2] == (std::pair<int, int>(9, -7)));
    BOOST_CHECK(points[3] == (std::pair<int, int>(10, -8)));
    BOOST_CHECK(points[4] == (std::pair<int, int>(11, -9)));
  }

  {
    // NNW line.
    RememberPoints<int> points;
    points = midpointLine(-8, 5, -11, 9, points);
    BOOST_CHECK(points.size() == 5);
    BOOST_CHECK(points[0] == (std::pair<int, int>(-8, 5)));
    BOOST_CHECK(points[1] == (std::pair<int, int>(-9, 6)));
    BOOST_CHECK(points[2] == (std::pair<int, int>(-9, 7)));
    BOOST_CHECK(points[3] == (std::pair<int, int>(-10, 8)));
    BOOST_CHECK(points[4] == (std::pair<int, int>(-11, 9)));
  }
}

template <class Integral>
void testCirclePoints(const geo::RememberPoints<Integral> &points,
                      const std::pair<Integral, Integral> &point)
{
  typedef std::pair<Integral, Integral> Point;
  Integral x = point.first;
  Integral y = point.second;

  BOOST_CHECK(std::find(points.begin(), points.end(), Point(x, y)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(x, -y)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(-x, -y)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(-x, y)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(y, x)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(y, -x)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(-y, -x)) != points.end());
  BOOST_CHECK(std::find(points.begin(), points.end(), Point(-y, x)) != points.end());
}

BOOST_AUTO_TEST_CASE(midpoint_circle_nr_points)
{
  using namespace geo;

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 0, points);
    BOOST_CHECK(points.size() == 1);
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 1, points);
    BOOST_CHECK(points.size() == 4);
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 0, 1, points);
    BOOST_CHECK(points.size() == 5);
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 2, points);
    BOOST_CHECK(points.size() == 12);
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 0, 2, points);
    BOOST_CHECK(points.size() == 21);
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 1, 2, points);
    BOOST_CHECK(points.size() == 20);
  }
}

BOOST_AUTO_TEST_CASE(midpoint_circle)
{
  using namespace geo;

  typedef std::pair<int, int> Point;

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 1, points);
    BOOST_CHECK(points.size() == 4);
    BOOST_CHECK(std::find(points.begin(), points.end(), Point(0, 1)) != points.end());
    BOOST_CHECK(std::find(points.begin(), points.end(), Point(1, 0)) != points.end());
    BOOST_CHECK(std::find(points.begin(), points.end(), Point(0, -1)) != points.end());
    BOOST_CHECK(std::find(points.begin(), points.end(), Point(-1, 0)) != points.end());
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 17, points);
    BOOST_CHECK(points.size() == 96);
    std::vector<Point> pointsInOctant;
    pointsInOctant.emplace_back(0, 17);
    pointsInOctant.emplace_back(1, 17);
    pointsInOctant.emplace_back(2, 17);
    pointsInOctant.emplace_back(3, 17);
    pointsInOctant.emplace_back(4, 17);
    pointsInOctant.emplace_back(5, 16);
    pointsInOctant.emplace_back(6, 16);
    pointsInOctant.emplace_back(7, 15);
    pointsInOctant.emplace_back(8, 15);
    pointsInOctant.emplace_back(9, 14);
    pointsInOctant.emplace_back(10, 14);
    pointsInOctant.emplace_back(11, 13);
    pointsInOctant.emplace_back(12, 12);
    pointsInOctant.emplace_back(17, 0);

    for (auto &it : pointsInOctant) {
      testCirclePoints(points, it);
    }
  }

  {
    RememberPoints<int> innerPoints;
    innerPoints = midpointCircle(0, 0, 13, innerPoints);
    BOOST_CHECK(innerPoints.size() == 72);
    std::vector<Point> pointsInOctant;
    pointsInOctant.emplace_back(0, 13);
    pointsInOctant.emplace_back(1, 13);
    pointsInOctant.emplace_back(2, 13);
    pointsInOctant.emplace_back(3, 13);
    pointsInOctant.emplace_back(4, 12);
    pointsInOctant.emplace_back(5, 12);
    pointsInOctant.emplace_back(6, 12);
    pointsInOctant.emplace_back(7, 11);
    pointsInOctant.emplace_back(8, 10);
    pointsInOctant.emplace_back(9, 9);

    for (auto &it : pointsInOctant) {
      testCirclePoints(innerPoints, it);
    }

    size_t const nrPointsInnerOctant =
        static_cast<size_t>(static_cast<double>(innerPoints.size()) / 8.0) - 1;
    BOOST_CHECK(nrPointsInnerOctant == 8);

    RememberPoints<int> outerPoints;
    outerPoints = midpointCircle(0, 0, 17, outerPoints);

    size_t const nrPointsOuterOctant =
        static_cast<size_t>(static_cast<double>(outerPoints.size()) / 8.0) - 1;
    BOOST_CHECK(nrPointsOuterOctant == 11);
  }

  {
    RememberPoints<int> points;
    points = midpointCircle(0, 0, 13, 17, points);
    BOOST_CHECK(points.size() == 460);

    std::vector<Point> pointsInOctant;

    // Inner circle.
    pointsInOctant.emplace_back(0, 13);
    pointsInOctant.emplace_back(1, 13);
    pointsInOctant.emplace_back(2, 13);
    pointsInOctant.emplace_back(3, 13);
    pointsInOctant.emplace_back(4, 12);
    pointsInOctant.emplace_back(5, 12);
    pointsInOctant.emplace_back(6, 12);
    pointsInOctant.emplace_back(7, 11);
    pointsInOctant.emplace_back(8, 10);
    pointsInOctant.emplace_back(9, 9);

    // Outer circle.
    pointsInOctant.emplace_back(0, 17);
    pointsInOctant.emplace_back(1, 17);
    pointsInOctant.emplace_back(2, 17);
    pointsInOctant.emplace_back(3, 17);
    pointsInOctant.emplace_back(4, 17);
    pointsInOctant.emplace_back(5, 16);
    pointsInOctant.emplace_back(6, 16);
    pointsInOctant.emplace_back(7, 15);
    pointsInOctant.emplace_back(8, 15);
    pointsInOctant.emplace_back(9, 14);
    pointsInOctant.emplace_back(10, 14);
    pointsInOctant.emplace_back(11, 13);
    pointsInOctant.emplace_back(12, 12);

    // Points between circles.
    pointsInOctant.emplace_back(0, 14);
    pointsInOctant.emplace_back(0, 15);
    pointsInOctant.emplace_back(0, 16);
    pointsInOctant.emplace_back(1, 14);
    pointsInOctant.emplace_back(1, 15);
    pointsInOctant.emplace_back(1, 16);
    pointsInOctant.emplace_back(2, 14);
    pointsInOctant.emplace_back(2, 15);
    pointsInOctant.emplace_back(2, 16);
    pointsInOctant.emplace_back(3, 14);
    pointsInOctant.emplace_back(3, 15);
    pointsInOctant.emplace_back(3, 16);
    pointsInOctant.emplace_back(4, 13);
    pointsInOctant.emplace_back(4, 14);
    pointsInOctant.emplace_back(4, 15);
    pointsInOctant.emplace_back(4, 16);
    pointsInOctant.emplace_back(5, 13);
    pointsInOctant.emplace_back(5, 14);
    pointsInOctant.emplace_back(5, 15);
    pointsInOctant.emplace_back(6, 13);
    pointsInOctant.emplace_back(6, 14);
    pointsInOctant.emplace_back(6, 15);
    pointsInOctant.emplace_back(7, 12);
    pointsInOctant.emplace_back(7, 13);
    pointsInOctant.emplace_back(7, 14);
    pointsInOctant.emplace_back(8, 11);
    pointsInOctant.emplace_back(8, 12);
    pointsInOctant.emplace_back(8, 13);
    pointsInOctant.emplace_back(8, 14);
    pointsInOctant.emplace_back(9, 10);
    pointsInOctant.emplace_back(9, 11);
    pointsInOctant.emplace_back(9, 12);
    pointsInOctant.emplace_back(9, 13);
    pointsInOctant.emplace_back(10, 10);
    pointsInOctant.emplace_back(10, 11);
    pointsInOctant.emplace_back(10, 12);
    pointsInOctant.emplace_back(10, 13);
    pointsInOctant.emplace_back(11, 11);
    pointsInOctant.emplace_back(11, 12);

    for (auto &it : pointsInOctant) {
      testCirclePoints(points, it);
    }
  }
}
