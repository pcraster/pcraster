#define BOOST_TEST_MODULE pcraster geo riks_neighbourhood
#include <boost/test/unit_test.hpp>
#include <boost/math/tr1.hpp>
#include "com_math.h"
#define protected public
#include "geo_riksneighbourhood.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  {
    // Nr 1.
    RiksNeighbourhood neighbourhood(0.0);
    BOOST_CHECK(neighbourhood.cell(0, 0) == 1.0);
  }

  {
    // Nr 2.
    RiksNeighbourhood neighbourhood(1.0, 1.0);
    BOOST_CHECK(neighbourhood.cell(0, 1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1, 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1, 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2, 1) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 4.0);
  }

  {
    // Nr 4.
    RiksNeighbourhood neighbourhood(2.0, 2.0);
    BOOST_CHECK(neighbourhood.cell(0, 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2, 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2, 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(4, 2) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 4.0);
  }

  {
    // Nr 13.
    double radius = boost::math::tr1::hypot(4.0, 2.0);
    RiksNeighbourhood neighbourhood(radius, radius);
    size_t offset = neighbourhood.radius();
    BOOST_CHECK(neighbourhood.cell(offset + 4, offset + 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 2, offset + 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 2, offset + 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 4, offset + 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 4, offset - 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 2, offset - 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 2, offset - 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 4, offset - 2) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 8.0);
  }

  {
    // Nr 30.
    RiksNeighbourhood neighbourhood(8.0, 8.0);
    size_t offset = neighbourhood.radius();
    BOOST_CHECK(neighbourhood.cell(offset + 0, offset + 8) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 8, offset + 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 8, offset + 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 0, offset - 8) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 4.0);
  }
}
