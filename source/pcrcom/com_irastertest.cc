#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_IRASTERTEST
#include "com_irastertest.h"
#define INCLUDED_COM_IRASTERTEST
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
#ifndef INCLUDED_COM_RASTER
#include "com_raster.h"
#define INCLUDED_COM_RASTER
#endif



/*!
  \file
  This file contains the implementation of the IRasterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IRASTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::IRasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<IRasterTest> instance(new IRasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&IRasterTest::testOutside, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IRasterTest::testGetCell, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF IRASTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::IRasterTest::IRasterTest()
{
}



//! setUp
void com::IRasterTest::setUp()
{
}

//! tearDown
void com::IRasterTest::tearDown()
{
}



void com::IRasterTest::testOutside()
{
  Raster<UINT1> v(4,5,8);

  BOOST_CHECK(!v.isOutside( 0, 0));
  BOOST_CHECK( v.isOutside(-1, 0));
  BOOST_CHECK( v.isOutside(-1, -1));
  BOOST_CHECK( v.isOutside( 0, -1));
  BOOST_CHECK( v.isOutside( 4,  5));
  BOOST_CHECK( v.isOutside( 3,  6));
  BOOST_CHECK( v.isOutside( 4,  2));

  BOOST_CHECK(v.cell(3,4)==8);

  v.setMV(0,2);

  BOOST_CHECK(v.isMV(0,2));
  BOOST_CHECK(!v.isMV(3,4));
}


void com::IRasterTest::testGetCell()
{
  Raster<UINT1> v(4,5,8);
  v.setMV(0,2);

  BOOST_CHECK(!v.isMV(3,4));


  {
  int r=0,c=0;
  UINT1 result=10;
  BOOST_CHECK(v.cell(result,r,c));
  BOOST_CHECK(result==8);
  }

  {
  size_t r=0,c=0;
  UINT1 result=10;
  BOOST_CHECK(v.cell(result,r,c));
  BOOST_CHECK(result==8);
  }

  {
  int r=-1,c=0;
  UINT1 result=10;
  BOOST_CHECK(!v.cell(result,r,c));
  BOOST_CHECK(result==10); // untouched
  }
  {
   int r=0,c=2;
   UINT1 result=10;
   BOOST_CHECK(v.isMV(r,c));
   BOOST_CHECK(!v.cell(result,r,c));
   BOOST_CHECK(result==10);
  }

  {
   size_t r=0,c=2;
   UINT1 result=10;
   BOOST_CHECK(v.isMV(r,c));
   BOOST_CHECK(!v.cell(result,r,c));
   BOOST_CHECK(result==10);
  }
}
