#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_FIELDTEST
#include "calc_fieldtest.h"
#define INCLUDED_CALC_FIELDTEST
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

// Module headers.
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

/*!
  \file
  This file contains the implementation of the FieldTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FIELD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::FieldTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FieldTest> instance(new FieldTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FieldTest::testCtorNonSpatial, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FIELD MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::FieldTest::FieldTest()
{
}



//! setUp
void calc::FieldTest::setUp()
{
}

//! tearDown
void calc::FieldTest::tearDown()
{
}

void calc::FieldTest::testCtorNonSpatial()
{
  {
    REAL4 v;
    NonSpatial ns(VS_S,v);
    BOOST_CHECK(ns.cr()==CR_REAL4);
  }
  {
    INT4 vb[3]={2,MV_INT4,6};
    Spatial  sb(VS_N,vb,3);

    BOOST_CHECK(sb.cr()==CR_INT4);
    BOOST_CHECK(sb.nrValues()==3);
    double v;
    BOOST_CHECK(sb.getCell(v,0));
    BOOST_CHECK(v==2);
    BOOST_CHECK(!sb.getCell(v,1));
    BOOST_CHECK(sb.getCell(v,2));
    BOOST_CHECK(v==6);
  }
}
