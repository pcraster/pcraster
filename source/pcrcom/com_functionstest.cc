#define BOOST_TEST_MODULE pcraster com clone
#include <boost/test/unit_test.hpp>
#include "com_functions.h"

#include <cmath>

BOOST_AUTO_TEST_CASE(minimum_)
{
  using namespace com;

  double min = NAN;
  double mv = NAN;
  pcr::setMV(mv);

  {
    double from[] = {2.0, 5.0, 3.0};
    min = minimum(from, from + 3);
    BOOST_TEST(min == 2.0);
  }

  {
    double from[] = {mv, mv, mv};
    min = minimum(from, from + 3);
    BOOST_TEST(pcr::isMV(min));
  }
}

BOOST_AUTO_TEST_CASE(maximum_)
{
  using namespace com;

  double max = NAN;
  double mv = NAN;
  pcr::setMV(mv);

  {
    double from[] = {2.0, 5.0, 3.0};
    max = maximum(from, from + 3);
    BOOST_TEST(max == 5.0);
  }

  {
    double from[] = {mv, mv, mv};
    max = maximum(from, from + 3);
    BOOST_TEST(pcr::isMV(max));
  }
}
