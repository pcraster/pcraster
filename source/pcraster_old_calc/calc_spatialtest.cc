#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_SPATIALTEST
#include "calc_spatialtest.h"
#define INCLUDED_CALC_SPATIALTEST
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
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif



/*!
  \file
  This file contains the implementation of the SpatialTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPATIAL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::SpatialTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpatialTest> instance(new SpatialTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SpatialTest::testSetAndGetCell, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPATIAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::SpatialTest::SpatialTest()
{
}



//! setUp
void calc::SpatialTest::setUp()
{
}

//! tearDown
void calc::SpatialTest::tearDown()
{
}



void calc::SpatialTest::testSetAndGetCell()
{
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
