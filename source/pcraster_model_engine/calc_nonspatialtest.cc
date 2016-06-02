#define BOOST_TEST_MODULE pcraster model_engine nonspatial
#include <boost/test/unit_test.hpp>
#include "com_csfcell.h"
#include "calc_nonspatial.h"
#include "calc_domainerror.h"

BOOST_AUTO_TEST_CASE(testSetAndGetCell)
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
    UINT1 v=0;
    NonSpatial n(VS_B,v);
    BOOST_CHECK(n.cri()==CRI_1);
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
    // setCell only used in lookup
    NonSpatial n(VS_N, 4);
    try {
     double v;
     n.getCell(v,81);
     BOOST_CHECK(v==4);
     // set some cell index
     n.setCell(-2,88);
     // get some cell index
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
    } catch(const DomainError& ) {
      catched=true;;
    }
    BOOST_CHECK(catched);
    BOOST_CHECK(n.getValue() == 8);
  }
}
