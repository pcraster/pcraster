#define BOOST_TEST_MODULE pcraster model_engine field
#include <boost/test/unit_test.hpp>
#include "calc_nonspatial.h"
#include "calc_spatial.h"

BOOST_AUTO_TEST_CASE(testCtorNonSpatial)
{
  using namespace calc;

  {
    REAL4 v;
    NonSpatial ns(VS_S,v);
    BOOST_CHECK(ns.cr()==CR_REAL4);
  }
  {
    INT4 vb[3]={2,MV_INT4,6};
    Spatial  sb(VS_N,vb,3);

    BOOST_CHECK(sb.cr()==CR_INT4);
    BOOST_CHECK(sb.nrValues()==3);
    double v;
    BOOST_CHECK(sb.getCell(v,0));
    BOOST_CHECK(v==2);
    BOOST_CHECK(!sb.getCell(v,1));
    BOOST_CHECK(sb.getCell(v,2));
    BOOST_CHECK(v==6);
  }
}
