#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READWRITEDATATEST
#include "fieldapi_readwritedatatest.h"
#define INCLUDED_FIELDAPI_READWRITEDATATEST
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
#ifndef INCLUDED_FIELDAPI_READWRITEDATA
#include "fieldapi_readwritedata.h"
#define INCLUDED_FIELDAPI_READWRITEDATA
#endif
#ifndef INCLUDED_FIELDAPI_TESTFIELD
#include "fieldapi_testfield.h"
#define INCLUDED_FIELDAPI_TESTFIELD
#endif


/*!
  \file
  This file contains the implementation of the ReadWriteDataTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC READWRITEDATA MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*fieldapi::ReadWriteDataTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ReadWriteDataTest> instance(new ReadWriteDataTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ReadWriteDataTest::testAll, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF READWRITEDATA MEMBERS
//------------------------------------------------------------------------------

//! ctor
fieldapi::ReadWriteDataTest::ReadWriteDataTest()
{
}

template<class UseAsT,class StoredAsT>
void fieldapi::ReadWriteDataTest::testType()
{
   StoredAsT  d1[ ] = { 0, 1, 2 , 3, 40, 5 };
   pcr::setMV(d1[4]);
   TestField<StoredAsT,2,3> d2(d1);
   ReadWriteData<UseAsT,StoredAsT> t(TEST_FIELD_INIT(d2));

   BOOST_CHECK(t.nrRows()==2);
   BOOST_CHECK(t.nrCols()==3);

   UseAsT v;

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

   for (size_t r=0; r < t.nrRows(); r++)
    for (size_t c=0; c < t.nrCols(); c++) {
      if (t.get(v,r,c)) {
        t.put(v*10,r,c);
        BOOST_CHECK(t.value(r,c) == v*10);

        t.putMV(r,c);
        BOOST_CHECK(!t.get(v,r,c));

        t.put(v,r,c);
        BOOST_CHECK(t.value(r,c) == v);
      }
    }

   t.putAllMV();
   for (size_t r=0; r < t.nrRows(); r++)
    for (size_t c=0; c < t.nrCols(); c++)
        BOOST_CHECK(!t.get(v,r,c));
}

//! testall
void fieldapi::ReadWriteDataTest::testAll()
{
 testType<UINT1,UINT1>();
 testType<INT4, UINT1>();
  testType<INT4, INT4>();
 testType<REAL8,UINT1>();
  testType<REAL8,INT4>();
 testType<REAL8,REAL4>();
}
