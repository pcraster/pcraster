#define BOOST_TEST_MODULE pcraster com single_valued_raster
#include <boost/test/unit_test.hpp>
#include "csftypes.h"
#include "com_singlevaluedraster.h"


BOOST_AUTO_TEST_CASE(this_)
{
  using namespace com;

  SingleValuedRaster<UINT1> v(4,5,8);
  BOOST_CHECK(v.nrRows()==4);
  BOOST_CHECK(v.nrCols()==5);
  BOOST_CHECK(v.cell(0,0)==8);
  BOOST_CHECK(v.cell(3,4)==8);

  v.setMV(0,2);

  BOOST_CHECK(v.cell(0,0)==MV_UINT1);
  BOOST_CHECK(v.isMV(0,0));
  BOOST_CHECK(v.isMV(2,0));
}


BOOST_AUTO_TEST_CASE(i_raster)
{
  using namespace com;
}
