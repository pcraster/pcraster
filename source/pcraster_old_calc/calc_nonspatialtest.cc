#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_NONSPATIALTEST
#include "calc_nonspatialtest.h"
#define INCLUDED_CALC_NONSPATIALTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
// Module headers.
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif



/*!
  \file
  This file contains the implementation of the NonSpatialTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC NONSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::NonSpatialTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<NonSpatialTest> instance(new NonSpatialTest());

  suite->add(BOOST_CLASS_TEST_CASE(&NonSpatialTest::testSetAndGetCell, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF NONSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::NonSpatialTest::NonSpatialTest()
{
}



//! setUp
void calc::NonSpatialTest::setUp()
{
}

//! tearDown
void calc::NonSpatialTest::tearDown()
{
}



void calc::NonSpatialTest::testSetAndGetCell()
{
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
