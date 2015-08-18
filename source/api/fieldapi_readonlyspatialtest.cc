#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READONLYSPATIALTEST
#include "fieldapi_readonlyspatialtest.h"
#define INCLUDED_FIELDAPI_READONLYSPATIALTEST
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
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_READONLYSPATIAL
#include "fieldapi_readonlyspatial.h"
#define INCLUDED_FIELDAPI_READONLYSPATIAL
#endif
#ifndef INCLUDED_FIELDAPI_TESTFIELD
#include "fieldapi_testfield.h"
#define INCLUDED_FIELDAPI_TESTFIELD
#endif



/*!
  \file
  This file contains the implementation of the ReadOnlySpatialTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC READONLYSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*fieldapi::ReadOnlySpatialTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ReadOnlySpatialTest> instance(new ReadOnlySpatialTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ReadOnlySpatialTest::testAll, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF READONLYSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
fieldapi::ReadOnlySpatialTest::ReadOnlySpatialTest()
{
}

template<typename UseAsT,typename StoredAsT>
  void fieldapi::ReadOnlySpatialTest::testType()
{
 StoredAsT  d1[ ] = { 0, 1, 2 , 3, 40, 5 };
 pcr::setMV(d1[4]);
 TestField<StoredAsT,2,3> d2(d1);
 ReadOnlySpatial<UseAsT,StoredAsT> t(TEST_FIELD_INIT(d2));
 UseAsT v;

 BOOST_CHECK(t.spatial());
 BOOST_CHECK(t.nrRows()==2);
 BOOST_CHECK(t.nrCols()==3);

 // out of range
 BOOST_CHECK(!t.get(v, 2,3));
 BOOST_CHECK(!t.get(v,-1,0));


 // get value
 BOOST_CHECK(t.get(v,0,0)); BOOST_CHECK(v==0);
 BOOST_CHECK(t.get(v,0,1)); BOOST_CHECK(v==1);
 BOOST_CHECK(t.get(v,0,2)); BOOST_CHECK(v==2);
 BOOST_CHECK(t.get(v,1,0)); BOOST_CHECK(v==3);
 BOOST_CHECK(t.get(v,1,2)); BOOST_CHECK(v==5);

 // get MV
 BOOST_CHECK(!t.get(v,1,1));

 // value
 BOOST_CHECK(t.value(0,0)==0);
 BOOST_CHECK(t.value(0,1)==1);
 BOOST_CHECK(t.value(0,2)==2);
 BOOST_CHECK(t.value(1,0)==3);
 BOOST_CHECK(t.value(1,2)==5);
}

//! test all
void fieldapi::ReadOnlySpatialTest::testAll()
{
 testType<UINT1,UINT1>();

 testType<INT4, UINT1>();
 testType<INT4, INT4>();

 testType<REAL8,UINT1>();
 testType<REAL8,INT4>();
 testType<REAL8,REAL4>();
}
