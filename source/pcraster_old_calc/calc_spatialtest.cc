#define BOOST_TEST_MODULE pcraster old_calc spatial
#include <boost/test/unit_test.hpp>
#include "com_csfcell.h"
#include "calc_spatial.h"


BOOST_AUTO_TEST_CASE(set_and_get_cell)
{
  using namespace calc;

  { // set a cell value scalar
    Spatial n(VS_S,5,true);
    try {
     double v;
     n.setCell(8,3);
     BOOST_CHECK(n.getCell(v,3));
     BOOST_CHECK(v==8);
    } catch(...) {
      bool success=false;
      BOOST_CHECK(success);
    }
  }

  { // set a cell boolean UINT1
    Spatial n(VS_B, 5,true);
    try {
     double v;
     n.setCell(1,3);
     BOOST_CHECK(n.getCell(v,3));
     BOOST_CHECK(v==1);
    } catch(...) {
      bool success=false;
      BOOST_CHECK(success);
    }
  }
  { // set a cell nominal INT4
    Spatial n(VS_N, 5,true);
    try {
     double v;
     n.setCell(-2,3);
     BOOST_CHECK(n.getCell(v,3));
     BOOST_CHECK(v==-2);
    } catch(...) {
      bool success=false;
      BOOST_CHECK(success);
    }
  }

  { // set a MV to VS_S
    Spatial n(VS_S, 5,true);
    double v;
    pcr::setMV(v);
    n.setCell(v,3);
    double r=4;
    BOOST_CHECK(!n.getCell(r,3));
    BOOST_CHECK(pcr::isMV(r));
   }
}
