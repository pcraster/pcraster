#define BOOST_TEST_MODULE pcraster dal spatial_coordinate
#include <boost/test/unit_test.hpp>
#include "dal_SpatialCoordinate.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

  {
    SpatialCoordinate const coordinate;
    BOOST_CHECK_CLOSE(coordinate.x(), 0.0, 0.001);
    BOOST_CHECK_CLOSE(coordinate.y(), 0.0, 0.001);
  }

  {
    SpatialCoordinate const coordinate(5.5, 6.6);
    BOOST_CHECK_CLOSE(coordinate.x(), 5.5, 0.001);
    BOOST_CHECK_CLOSE(coordinate.y(), 6.6, 0.001);
  }
}


BOOST_AUTO_TEST_CASE(copy)
{
  using namespace dal;

  {
    SpatialCoordinate const coordinate1;
    const SpatialCoordinate& coordinate2(coordinate1);

    BOOST_CHECK_CLOSE(coordinate2.x(), 0.0, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 0.0, 0.001);
  }

  {
    SpatialCoordinate const coordinate1(5.5, 6.6);
    const SpatialCoordinate& coordinate2(coordinate1);

    BOOST_CHECK_CLOSE(coordinate2.x(), 5.5, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 6.6, 0.001);
  }
}


BOOST_AUTO_TEST_CASE(assignment)
{
  using namespace dal;

  {
    SpatialCoordinate const coordinate1;
    const SpatialCoordinate& coordinate2 = coordinate1;

    BOOST_CHECK_CLOSE(coordinate2.x(), 0.0, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 0.0, 0.001);
  }

  {
    SpatialCoordinate const coordinate1(5.5, 6.6);
    const SpatialCoordinate& coordinate2 = coordinate1;

    BOOST_CHECK_CLOSE(coordinate2.x(), 5.5, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 6.6, 0.001);
  }
}


BOOST_AUTO_TEST_CASE(equals)
{
  using namespace dal;

  SpatialCoordinate const coordinate1;
  SpatialCoordinate const coordinate2;
  SpatialCoordinate const coordinate3(5.5, 6.6);
  SpatialCoordinate const coordinate4(5.5, 6.6);

  BOOST_CHECK(coordinate1 == coordinate2);
  BOOST_CHECK(coordinate1 != coordinate3);
  BOOST_CHECK(coordinate3 == coordinate4);
}
