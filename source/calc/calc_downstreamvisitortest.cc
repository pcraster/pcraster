#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DOWNSTREAMVISITORTEST
#include "calc_downstreamvisitortest.h"
#define INCLUDED_CALC_DOWNSTREAMVISITORTEST
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
#ifndef INCLUDED_FIELDAPI_TESTFIELD
#include "fieldapi_testfield.h"
#define INCLUDED_FIELDAPI_TESTFIELD
#endif
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif

// Module headers.
#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
#endif



/*!
  \file
  This file contains the implementation of the DownStreamVisitorTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::DownStreamVisitorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DownStreamVisitorTest> instance(new DownStreamVisitorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DownStreamVisitorTest::testPitOnly, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DownStreamVisitorTest::testAllDirs, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DownStreamVisitorTest::testKnownOrder, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DownStreamVisitorTest::testRealisticLdd, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::DownStreamVisitorTest::DownStreamVisitorTest()
{
}


//! a realistic ldd
void calc::DownStreamVisitorTest::testRealisticLdd()
{
 {
  UINT1 mask1[]={ MV_UINT1, 2,        2, MV_UINT1, 1,
                  5,        4,        4,        4, MV_UINT1,
                  MV_UINT1, 6,        9, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> mask2(mask1);
  fieldapi::ReadWriteData<UINT1,UINT1> mask(TEST_FIELD_INIT(mask2));
  mask.putAllMV();

  UINT1 d1[]={ MV_UINT1, 2,  2, MV_UINT1, 1,
               5,        4,  4, 4,        MV_UINT1,
               MV_UINT1, 6,  9, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));


  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,0)); v.valid(); ++v) {
     for(geo::UpstreamNeighbourVisitor u(*v); u.valid(); ++u)
        BOOST_CHECK(mask.value(u.nb())==1);

     count++;
     UINT1 m;
     BOOST_CHECK(!mask.get(m,*v));
     mask.put(1,*v);
  }
  BOOST_CHECK(count==9);
 }
}

//! ldd's with single upstream neighbour: known order
void calc::DownStreamVisitorTest::testKnownOrder()
{
 {
  UINT1 d1[]={ 5, 4, 4 };
  fieldapi::TestField<UINT1,1,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(0,0)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(0,2-count));
     count++;
  }
  BOOST_CHECK(count==3);
 }
 {
  UINT1 d1[]={ MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1,  5, 4, 4, MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,1)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(1,3-count));
     count++;
  }
  BOOST_CHECK(count==3);
 }
 {
  UINT1 d1[]={ 6, 6, 5 };
  fieldapi::TestField<UINT1,1,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(0,2)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(0,count));
     count++;
  }
  BOOST_CHECK(count==3);
 }
 {
  UINT1 d1[]={ MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1,  6, 6, 5, MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,3)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(1,count+1));
     count++;
  }
  BOOST_CHECK(count==3);
 }
}

//! check all directions
void calc::DownStreamVisitorTest::testAllDirs()
{
 {
  UINT1 d1[]={ 3, 2, 1,
               6, 5, 4,
               9, 8, 7 };
  fieldapi::TestField<UINT1,3,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,1)); v.valid(); ++v) {
     count++;
  }
  BOOST_CHECK(count==9);
 }
 {
  UINT1 d1[]= {MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1, 3, 2, 1, MV_UINT1,
               MV_UINT1, 6, 5, 4, MV_UINT1,
               MV_UINT1, 9, 8, 7, MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,5,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(2,2)); v.valid(); ++v) {
     count++;
  }
  BOOST_CHECK(count==9);
 }
}


//! test case of pit only
void calc::DownStreamVisitorTest::testPitOnly()
{
 {
  UINT1 d1[]={ 5 };
  fieldapi::TestField<UINT1,1,1> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));


  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(0,0)); v.valid(); ++v) {
     count++;
  }
  BOOST_CHECK(count==1);
 }
 {
  UINT1 d1[]={ MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1,   5     , MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  bool shouldNotReach=true;
  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,1)); v.valid(); ++v) {
     count++;
     for(geo::UpstreamNeighbourVisitor u(*v); u.valid(); ++u)
        shouldNotReach=false;
  }
  BOOST_CHECK(shouldNotReach);
  BOOST_CHECK(count==1);
 }
// test with   2*2 -> all MV not possible,yet
/*
 test with   1*1 -> pit
 */
}

