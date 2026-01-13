#define BOOST_TEST_MODULE pcraster aguila util
#include <boost/test/unit_test.hpp>
#include "com_util.h"

BOOST_AUTO_TEST_CASE(smallest_divisor)
{
  using namespace com;

  BOOST_TEST(smallestDivisor(10, 1) == 1);
  BOOST_TEST(smallestDivisor(10, 2) == 2);
  BOOST_TEST(smallestDivisor(10, 3) == 5);
  BOOST_TEST(smallestDivisor(10, 4) == 5);
  BOOST_TEST(smallestDivisor(10, 6) == 10);
  BOOST_TEST(smallestDivisor(10, 10) == 10);
}

BOOST_AUTO_TEST_CASE(largest_divisor)
{
  using namespace com;

  BOOST_TEST(largestDivisor(10, 10) == 10);
  BOOST_TEST(largestDivisor(10, 9) == 5);
  BOOST_TEST(largestDivisor(10, 6) == 5);
  BOOST_TEST(largestDivisor(10, 5) == 5);
  BOOST_TEST(largestDivisor(10, 4) == 2);
  BOOST_TEST(largestDivisor(10, 2) == 2);
  BOOST_TEST(largestDivisor(10, 1) == 1);
}
