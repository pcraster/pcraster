#define BOOST_TEST_MODULE pcraster com algorithm
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_binaryoperators.h"


BOOST_AUTO_TEST_CASE(mv_cast)
{
  using namespace com;

  UINT1 to[] = {  2,  5,  3};
  UINT1 from = MV_UINT1;

 addValue(to, to+3, from);

 BOOST_CHECK(to[0]==MV_UINT1);
}


BOOST_AUTO_TEST_CASE(divide)
{
  using namespace com;

  double to[]   = { 10.0, 20.0, 30.0 };
  double from[] = {  2.0,  5.0,  3.0 };

  divideByRange(to, to + 3, from);

  BOOST_CHECK(to[0] ==  5.0);
  BOOST_CHECK(to[1] ==  4.0);
  BOOST_CHECK(to[2] == 10.0);

  divideByValue(to, to + 3, 2.0);
  BOOST_CHECK(to[0] == 2.5);
  BOOST_CHECK(to[1] == 2.0);
  BOOST_CHECK(to[2] == 5.0);

}
