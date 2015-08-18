#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
#include "calc_iobandfieldstrategytest.h"
#define INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
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
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif

// Module headers.
#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGY
#include "calc_iobandfieldstrategy.h"
#define INCLUDED_CALC_IOBANDFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_BANDMAP
#include "calc_bandmap.h"
#define INCLUDED_CALC_BANDMAP
#endif
/*!
  \file
  This file contains the implementation of the IoBandFieldStrategyTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOBANDFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::IoBandFieldStrategyTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<IoBandFieldStrategyTest> instance(new IoBandFieldStrategyTest());

  suite->add(BOOST_CLASS_TEST_CASE(&IoBandFieldStrategyTest::testCheckInputMap, instance));
  // CheckClone depends on CheckInputMap
  suite->add(BOOST_CLASS_TEST_CASE(&IoBandFieldStrategyTest::testCheckClone, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoBandFieldStrategyTest::testCreateMap, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF IOBANDFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoBandFieldStrategyTest::IoBandFieldStrategyTest(){
}



//! setUp
void calc::IoBandFieldStrategyTest::setUp()
{
 { // create a  4*5 UINT1 map as clone
   com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\nYDIM 0.5",
   com::PathName("clone.hdr"));
   UINT1 buf[20];
   std::generate_n(buf,20,com::SeqInc<UINT1>());
   com::write(buf,20,com::PathName("clone.bil"));
 }
 { // create a minimal 5*4 different loc. attr.
   com::write("NROWS 5\nNCOLS 4\n",
   com::PathName("diffLocAttr.hdr"));
   UINT1 buf[20];
   std::generate_n(buf,20,com::SeqInc<UINT1>());
   com::write(buf,20,com::PathName("diffLocAttr.bil"));
 }
}

//! tearDown
void calc::IoBandFieldStrategyTest::tearDown()
{
}

void calc::IoBandFieldStrategyTest::testCheckClone()
{
  IoBandFieldStrategy s;
  bool succes=true;
  try {
   // nothing to test against
   s.checkClone("clone");
  } catch (...) {
   succes=false;
  }
  try {
   // the same is equal
   s.checkClone("clone");
  } catch (...) {
   succes=false;
  }
  BOOST_CHECK(succes);

  bool failure=false;
  try {
   // different
   s.checkClone("diffLocAttr");
  } catch (...) {
   failure=true;
  }
  BOOST_CHECK(failure);
}

void calc::IoBandFieldStrategyTest::testCheckInputMap()
{
  VS vs;
  IoBandFieldStrategy s;
  IoFieldStrategy *r(0);
  bool succes=true;
  try {
   r = s.checkInputMap(vs,"clone");
  } catch (...) {
   succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(vs==VS_BNO);
  BOOST_CHECK(&s == r);

  try {
   r = s.checkInputMap(vs,"diffLocAttr");
  } catch (...) {
   succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(vs==VS_BNO);
  BOOST_CHECK(&s == r);

  bool failure=false;
  try {
    s.checkInputMap(vs,"failureExpectedBILNotExistant");
  } catch (...) {
   failure=true;
  }
  BOOST_CHECK(failure);
}

void calc::IoBandFieldStrategyTest::testCreateMap()
{
  IoBandFieldStrategy s;
  s.checkClone("clone");
  geo::BandMap clone("clone");

 {
  UINT1 v=3;
  GridMap *gm=s.createMap("uint1Bool",VS_B);
  gm->writeNonSpatial(&v);
  delete gm;

  geo::BandMap bm("uint1Bool");
  BOOST_CHECK(bm.nrCells() == 20);
  BOOST_CHECK(bm.rasterSpace() == clone.rasterSpace());
  BOOST_CHECK(bm.cellRepr() == CR_UINT1);
  UINT1 cells[20];
  bm.getCellsRaw(cells);
  BOOST_CHECK(cells[0] == 3);
  BOOST_CHECK(cells[17] == 3);
 }

 {
  INT4 v=3;
  GridMap *gm=s.createMap("int4",VS_N);
  gm->writeNonSpatial(&v);
  delete gm;

  geo::BandMap bm("int4");
  BOOST_CHECK(bm.nrCells() == 20);
  BOOST_CHECK(bm.rasterSpace() == clone.rasterSpace());
  BOOST_CHECK(bm.cellRepr() == CR_INT2);
  INT4 cells[20];
  bm.getCellsAsINT4(cells);
  BOOST_CHECK(cells[0] == 3);
  BOOST_CHECK(cells[17] == 3);
 }

 {
  REAL4 v=3;
  GridMap *gm=s.createMap("real4",VS_S);
  gm->writeNonSpatial(&v);
  delete gm;

  geo::BandMap bm("real4");
  BOOST_CHECK(bm.nrCells() == 20);
  BOOST_CHECK(bm.rasterSpace() == clone.rasterSpace());
  BOOST_CHECK(bm.cellRepr() == CR_REAL4);
  REAL4 cells[20];
  bm.getCellsAsREAL4(cells);
  BOOST_CHECK(cells[0] == 3);
  BOOST_CHECK(cells[17] == 3);
 }

 { // assume the largest
  REAL4 v=3;
  GridMap *gm=s.createMap("real4",VS_FIELD);
  gm->writeNonSpatial(&v);
  delete gm;

  geo::BandMap bm("real4");
  BOOST_CHECK(bm.nrCells() == 20);
  BOOST_CHECK(bm.rasterSpace() == clone.rasterSpace());
  BOOST_CHECK(bm.cellRepr() == CR_REAL4);
  REAL4 cells[20];
  bm.getCellsAsREAL4(cells);
  BOOST_CHECK(cells[0]  == 3);
  BOOST_CHECK(cells[17] == 3);
 }

}
