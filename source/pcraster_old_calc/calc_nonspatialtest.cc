#define BOOST_TEST_MODULE pcraster old_calc non_spatial
#include <boost/test/unit_test.hpp>
#include "com_csfcell.h"
#include "calc_nonspatial.h"


BOOST_AUTO_TEST_CASE(set_and_get_cell)
{
  using namespace calc;

  { // set a value scalar
    NonSpatial n(VS_S,4);
    try {
     double v;
     n.getCell(v,81);
     BOOST_CHECK(v==4);
     n.setCell(8,88);
     n.getCell(v,79);
     BOOST_CHECK(v==8);
    } catch(...) {
      bool success=false;
      BOOST_CHECK(success);
    }
    BOOST_CHECK(n.getValue() == 8);
  }

  { // set a value boolean UINT1
    NonSpatial n(VS_B, 0);
    try {
     double v;
     n.getCell(v,81);
     BOOST_CHECK(v==0);
     n.setCell(1,88);
     n.getCell(v,79);
     BOOST_CHECK(v==1);
    } catch(...) {
      bool success=false;
      BOOST_CHECK(success);
    }
    BOOST_CHECK(n.getValue() == 1);
  }
  { // set a value nominal INT4
    NonSpatial n(VS_N, 4);
    try {
     double v;
     n.getCell(v,81);
     BOOST_CHECK(v==4);
     n.setCell(-2,88);
     n.getCell(v,79);
     BOOST_CHECK(v==-2);
    } catch(...) {
      bool success=false;
      BOOST_CHECK(success);
    }
    BOOST_CHECK(n.getValue() == -2);
  }

  { // set a MV
    NonSpatial n(VS_S, 8);
    double v;
    pcr::setMV(v);
    bool catched=false;
    try {
     n.setCell(v,88);
    } catch(const Field::SetNonSpatialToMV& v) {
      catched=true;;
    }
    BOOST_CHECK(catched);
    BOOST_CHECK(n.getValue() == 8);
  }
}
