#define BOOST_TEST_MODULE pcraster com math
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_math.h"


BOOST_AUTO_TEST_CASE(epsilon)
{
  using namespace com;

  // one of them really 0 makes fraction BIG
  BOOST_CHECK( equal_epsilon<double>(0, 0.00001, 0.00001));

  BOOST_CHECK(equal_epsilon<double>(1,1));
  BOOST_CHECK(!equal_epsilon<double>(1,2));

  BOOST_CHECK(equal_epsilon<double>(1, 1.00001, 0.1));
  BOOST_CHECK(equal_epsilon<double>(1, 1.00001, 0.00001));

  BOOST_CHECK( equal_epsilon<double>(5, 5.00001, 0.00001));
  BOOST_CHECK( equal_epsilon<double>(5, 5.00001, 0.000005));
  BOOST_CHECK(!equal_epsilon<double>(5, 5.00001, 0.0000005));

  BOOST_CHECK(equal_epsilon<double>(std::sqrt(2.0), 1.41, 0.1));
  BOOST_CHECK(equal_epsilon<double>(std::sqrt(2.0), 1.41, 0.01));

  BOOST_CHECK(fractionDifference<double>(2, 2)==0);

  BOOST_CHECK(fractionDifference<double>(2, 1)==0.5);
  BOOST_CHECK(fractionDifference<double>(1, 2)==0.5);
}


BOOST_AUTO_TEST_CASE(is_integer)
{
  using namespace com;

  BOOST_CHECK(isInteger(0.0));
  BOOST_CHECK(isInteger(-100.0));
  BOOST_CHECK(isInteger(  99.0));

  BOOST_CHECK(!isInteger(0.000001));
  BOOST_CHECK(!isInteger(-99.000007));
  BOOST_CHECK(!isInteger(111.07));
}


BOOST_AUTO_TEST_CASE(interpolate2_)
{
  using namespace com;

  BOOST_CHECK(interpolate2(1.5, 1, 3, 2, 4) == 3.5);
  BOOST_CHECK(interpolate2(1.5, 2, 4, 1, 3) == 3.5);
  BOOST_CHECK(interpolate2(0, -1, -1, 1, 1) == 0);
  BOOST_CHECK(interpolate2(0, -3, -3, 1, 1) == 0);
  BOOST_CHECK(equal_epsilon<double>(interpolate2(0, -4, 0, 1, 1),0.8));

  BOOST_CHECK(interpolate2(0, -3, -3, -3, -3) == -3);
  BOOST_CHECK(interpolate2(0, 0, 0, 1, 1) == 0);
}


BOOST_AUTO_TEST_CASE(minimize_maximize)
{
  using namespace com;

  {
    int x=-1,y=3;
    maximize(x,y);
    BOOST_CHECK(x==3);
    maximize(x,8);
    BOOST_CHECK(x==8);
    minimize(x,y);
    BOOST_CHECK(x==3);
    minimize(x,-9);
    BOOST_CHECK(x==-9);
  }
  {
    double x=-1,y=3;
    maximize(x,y);
    BOOST_CHECK(x==3);
    maximize<double>(x,8);
    BOOST_CHECK(x==8);
    minimize(x,y);
    BOOST_CHECK(x==3);
    minimize(x,-9.0);
    BOOST_CHECK(x==-9);
  }
}


BOOST_AUTO_TEST_CASE(lim_)
{
  using namespace com;

  BOOST_CHECK(lim<int>(2,4,6) == 4);
  BOOST_CHECK(lim<int>(8,4,6) == 6);
  BOOST_CHECK(lim<int>(5,4,6) == 5);

  BOOST_CHECK(limUnordered<int>(2,4,6) == 4);
  BOOST_CHECK(limUnordered<int>(8,4,6) == 6);
  BOOST_CHECK(limUnordered<int>(5,4,6) == 5);

  BOOST_CHECK(limUnordered<int>(2,6,4) == 4);
  BOOST_CHECK(limUnordered<int>(8,6,4) == 6);
  BOOST_CHECK(limUnordered<int>(5,6,4) == 5);
}
