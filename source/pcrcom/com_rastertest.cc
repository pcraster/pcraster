#define BOOST_TEST_MODULE pcraster com raster
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "csftypes.h"
#include "com_raster.h"


BOOST_AUTO_TEST_CASE(constructor_single_value)
{
  using namespace com;

  Raster<UINT1> v(4,5,8);
  BOOST_CHECK(v.nrRows()==4);
  BOOST_CHECK(v.nrCols()==5);
  BOOST_CHECK(v.cell(0, 0)==8);
  BOOST_CHECK(v.cell(3,4)==8);

  v.setMV(0,2);
  BOOST_CHECK(v.isMV(0,2));
  BOOST_CHECK(!v.isMV(3,4));
}
