#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIALTEST
#include "fieldapi_readonlynonspatialtest.h"
#define INCLUDED_FIELDAPI_READONLYNONSPATIALTEST
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
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIAL
#include "fieldapi_readonlynonspatial.h"
#define INCLUDED_FIELDAPI_READONLYNONSPATIAL
#endif



/*!
  \file
  This file contains the implementation of the ReadOnlyNonSpatialTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC READONLYNONSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*fieldapi::ReadOnlyNonSpatialTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ReadOnlyNonSpatialTest> instance(new ReadOnlyNonSpatialTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ReadOnlyNonSpatialTest::testAll, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF READONLYNONSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
fieldapi::ReadOnlyNonSpatialTest::ReadOnlyNonSpatialTest()
{
}

template<typename T>
 void fieldapi::ReadOnlyNonSpatialTest::testType(T initVal)
{
  ReadOnlyNonSpatial<T> t(initVal,2,3);
  T v;
  BOOST_CHECK(!t.spatial());
  BOOST_CHECK(t.nrRows()==2);
  BOOST_CHECK(t.nrCols()==3);
  BOOST_CHECK(t.nrCols()==3);
  BOOST_CHECK(t.get(v,0,0));
  BOOST_CHECK(v==initVal);
  BOOST_CHECK(!t.get(v,2,3));
  BOOST_CHECK(!t.get(v,-1,0));
  BOOST_CHECK(t.value(0,0)==initVal);
}

//! test all
void fieldapi::ReadOnlyNonSpatialTest::testAll()
{
 testType<UINT1>(34);
 testType<INT4>(-4);
 testType<INT4>(1058);
 testType<REAL8>(0);
 testType<REAL8>(3.45);
}
