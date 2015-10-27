#define BOOST_TEST_MODULE pcraster aguila range_map
#include <boost/test/unit_test.hpp>
#include "com_rangemap.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace com;

  RangeMap<double, double> map(1.0, 2.0, 0.0, 100.0);
  BOOST_CHECK(map.map(1.5) == 50.0);

  map.setRanges(0.0, 0.0, 3.0, 100.0);
  BOOST_CHECK(map.map(1.5) == 3.0);

  map.setRanges(1.0, 2.0, 0.0, 0.0);
  BOOST_CHECK(map.map(1.5) == 0.0);
}
