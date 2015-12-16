#define BOOST_TEST_MODULE pcraster geo point_value
#include <boost/test/unit_test.hpp>
#include "geo_point.h"
#include "geo_pointvalue.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  {
    typedef Point<double, 2> Point;
    typedef double Value;

    Point point(2.2, 3.3);
    Value value(5.5);

    PointValue<Point, Value> height(point, value);
    BOOST_CHECK(height.point() == point);
    BOOST_CHECK(height.value() == value);

    point += 8.8;
    height.setPoint(point);
    BOOST_CHECK(height.point() == point);

    value += 9.9;
    height.setValue(value);
    BOOST_CHECK(height.value() == value);
  }
}
