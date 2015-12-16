#define BOOST_TEST_MODULE pcraster geo circular_neighbourhood
#include <boost/test/unit_test.hpp>
#include "geo_circularneighbourhood.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  {
    CircularNeighbourhood neighbourhood(1.0);
    BOOST_CHECK(neighbourhood.cell(0,0) == 0.0);
    BOOST_CHECK(neighbourhood.cell(0,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,2) == 0.0);
    BOOST_CHECK(neighbourhood.cell(1,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,0) == 0.0);
    BOOST_CHECK(neighbourhood.cell(2,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,2) == 0.0);
  }
}
