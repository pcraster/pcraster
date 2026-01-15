#define BOOST_TEST_MODULE pcraster model_engine field
#include <math.h>

#include <boost/test/unit_test.hpp>
#include "calc_nonspatial.h"
#include "calc_spatial.h"
#include <cmath>

BOOST_AUTO_TEST_CASE(testCtorNonSpatial)
{
  using namespace calc;

  {
    REAL4 const v = NAN;
    NonSpatial const ns(VS_S, v);
    BOOST_TEST(ns.cr() == CR_REAL4);
  }
  {
    INT4 vb[3] = {2, MV_INT4, 6};
    Spatial const sb(VS_N, vb, 3);

    BOOST_TEST(sb.cr() == CR_INT4);
    BOOST_TEST(sb.nrValues() == 3);
    double v = NAN;
    BOOST_TEST(sb.getCell(v, 0));
    BOOST_TEST(v == 2);
    BOOST_TEST(!sb.getCell(v, 1));
    BOOST_TEST(sb.getCell(v, 2));
    BOOST_TEST(v == 6);
  }
}
