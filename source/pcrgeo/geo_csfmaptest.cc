#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFMAPTEST
#include "geo_csfmaptest.h"
#define INCLUDED_GEO_CSFMAPTEST
#endif

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

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*geo::CSFMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CSFMapTest> instance(new CSFMapTest());

  // suite->addTest(new TestCaller<CSFMapTest>("testConstructor",
  //                  &geo::CSFMapTest::testConstructor));
  // suite->addTest(new TestCaller<CSFMapTest>("testNrRows",
  //                  &geo::CSFMapTest::testNrRows));
  // suite->addTest(new TestCaller<CSFMapTest>("testNrCols",
  //                  &geo::CSFMapTest::testNrCols));
  // suite->addTest(new TestCaller<CSFMapTest>("testNrCells",
  //                  &geo::CSFMapTest::testNrCells));
  //
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testNrRows, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testNrCols, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testNrCells, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

geo::CSFMapTest::CSFMapTest()

  :    d_map1(0)

{
}



void geo::CSFMapTest::setUp()
{
  d_map1 = new CSFMap("map1.map",11, 12, VS_SCALAR, PT_YINCT2B, 3.0, 14.0, 1.0, 5.0);
}



void geo::CSFMapTest::tearDown()
{
  delete d_map1;
  remove("map1.map");
}



void geo::CSFMapTest::testConstructor()
{
 // illegal angle
 CSFMap *map2(0);
 bool failureExpected=false;
 try {
    map2 = new CSFMap("csfmap2.map",11, 12, VS_SCALAR, PT_YINCT2B, 3.0, 14.0, 15.0, 5.0);
 } catch(const com::FileError& e) {
  const char *msgExpect =
    "File 'csfmap2.map': error creating raster: Angle < -0.5 pi or > 0.5 pi\n";
  BOOST_CHECK(e.messages() == msgExpect);
  failureExpected=true;
 }
 BOOST_CHECK(failureExpected);
 delete map2;
 BOOST_CHECK(!com::PathInfo("csfmap2.map").exists());
}



void geo::CSFMapTest::testNrRows()
{
  setUp();

  BOOST_CHECK(d_map1->nrRows() == 11);

  tearDown();
}



void geo::CSFMapTest::testNrCols()
{
  setUp();

  BOOST_CHECK(d_map1->nrCols() == 12);

  tearDown();
}



void geo::CSFMapTest::testNrCells()
{
  setUp();

  BOOST_CHECK(d_map1->nrCells() == 11 * 12);

  tearDown();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


