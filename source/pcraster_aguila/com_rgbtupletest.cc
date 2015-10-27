#define BOOST_TEST_MODULE pcraster aguila rgb_tuple
#include <boost/test/unit_test.hpp>
#include "com_rgbtuple.h"


BOOST_AUTO_TEST_CASE(eq)
{
  using namespace com;

  RgbTuple gray25(64,64,64);
  BOOST_CHECK(gray25 == gray25);
  BOOST_CHECK(gray25 != RgbTuple::gray50_);
  BOOST_CHECK(RgbTuple::gray50_ == RgbTuple::gray50_);
  BOOST_CHECK(RgbTuple::gray50_ != RgbTuple::black_);
}
