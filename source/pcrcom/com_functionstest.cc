#define BOOST_TEST_MODULE pcraster com clone
#include <boost/test/unit_test.hpp>
#include "com_functions.h"


BOOST_AUTO_TEST_CASE(minimum_)
{
  using namespace com;

  double min;
  double mv;
  pcr::setMV(mv);

  {
    double from[] = {  2.0,  5.0,  3.0 };
    min = minimum(from, from + 3);
    BOOST_CHECK(min ==  2.0);
  }

  {
    double from[] = { mv, mv, mv };
    min = minimum(from, from + 3);
    BOOST_CHECK(pcr::isMV(min));
  }
}


BOOST_AUTO_TEST_CASE(maximum_)
{
  using namespace com;

  double max;
  double mv;
  pcr::setMV(mv);

  {
    double from[] = {  2.0,  5.0,  3.0 };
    max = maximum(from, from + 3);
    BOOST_CHECK(max == 5.0);
  }

  {
    double from[] = { mv, mv, mv };
    max = maximum(from, from + 3);
    BOOST_CHECK(pcr::isMV(max));
  }
}
