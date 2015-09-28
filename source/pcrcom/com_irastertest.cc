#define BOOST_TEST_MODULE pcraster com iraster
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_raster.h"


BOOST_AUTO_TEST_CASE(outside)
{
  using namespace com;

  Raster<UINT1> v(4,5,8);

  BOOST_CHECK(!v.isOutside( 0, 0));
  BOOST_CHECK( v.isOutside(-1, 0));
  BOOST_CHECK( v.isOutside(-1, -1));
  BOOST_CHECK( v.isOutside( 0, -1));
  BOOST_CHECK( v.isOutside( 4,  5));
  BOOST_CHECK( v.isOutside( 3,  6));
  BOOST_CHECK( v.isOutside( 4,  2));

  BOOST_CHECK(v.cell(3,4)==8);

  v.setMV(0,2);

  BOOST_CHECK(v.isMV(0,2));
  BOOST_CHECK(!v.isMV(3,4));
}


BOOST_AUTO_TEST_CASE(get_cell)
{
  using namespace com;

  Raster<UINT1> v(4,5,8);
  v.setMV(0,2);

  BOOST_CHECK(!v.isMV(3,4));


  {
  int r=0,c=0;
  UINT1 result=10;
  BOOST_CHECK(v.cell(result,r,c));
  BOOST_CHECK(result==8);
  }

  {
  size_t r=0,c=0;
  UINT1 result=10;
  BOOST_CHECK(v.cell(result,r,c));
  BOOST_CHECK(result==8);
  }

  {
  int r=-1,c=0;
  UINT1 result=10;
  BOOST_CHECK(!v.cell(result,r,c));
  BOOST_CHECK(result==10); // untouched
  }
  {
   int r=0,c=2;
   UINT1 result=10;
   BOOST_CHECK(v.isMV(r,c));
   BOOST_CHECK(!v.cell(result,r,c));
   BOOST_CHECK(result==10);
  }

  {
   size_t r=0,c=2;
   UINT1 result=10;
   BOOST_CHECK(v.isMV(r,c));
   BOOST_CHECK(!v.cell(result,r,c));
   BOOST_CHECK(result==10);
  }
}
