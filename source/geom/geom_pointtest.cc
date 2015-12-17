#define BOOST_TEST_MODULE pcraster geom point
#include <boost/test/unit_test.hpp>
#include "geometry.h"


//! all tests where failures due to CmpEps (point.c) problems
BOOST_AUTO_TEST_CASE(perp_on_cord)
{
  {
     POINT2D  k = {1547.265625, 4.4459505081176758};
     LINE     l = {0, -3.4353947900726436e-08, 3.8499610257178518};
     LINE res;
     PerpLine(&res,&k,&l);
  }
  {
    POINT2D c1 = { -1, 1 };
    POINT2D c2 = {  1, -1 };
    POINT2D p  = {  1, 1 };
    POINT2D cut;
    LINE    perp;
    PerpOnCord(&cut,&perp,&p,&c1,&c2);
    BOOST_CHECK(cut.x == 0 && cut.y == 0);
    BOOST_CHECK(perp.yInt == 0 );
  }

  POINT2D p  = { 4354, 0 /* -8.4128132584737614e-06 */};
  POINT2D c1 = { 4373, 0 /* -0.7089846134185791 */ };
  POINT2D c2 = { 4344, -0.631000};
  POINT2D cut;
  LINE    perp;
  PerpOnCord(&cut,&perp,&p,&c1,&c2);


  {
  POINT2D p  = { 4354.73974609375, 0 /* -8.4128132584737614e-06 */};
  POINT2D c1 = { 4373.07275390625, 0 /* -0.7089846134185791 */ };
  POINT2D c2 = { 4344.58349609375, -0.63100546598434448};
  POINT2D cut;
  LINE    perp;
  PerpOnCord(&cut,&perp,&p,&c1,&c2);
  }

  POINT2D k = {4354.73974609375, -8.4128132584737614e-06};
  LINE    l= {0, -0.0027371421167742861, 11.260737001016331};
  LINE res;
  PerpLine(&res,&k,&l);
}
