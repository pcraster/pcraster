#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SINGLEVALUEDRASTERTEST
#include "com_singlevaluedrastertest.h"
#define INCLUDED_COM_SINGLEVALUEDRASTERTEST
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
#ifndef INCLUDED_COM_SINGLEVALUEDRASTER
#include "com_singlevaluedraster.h"
#define INCLUDED_COM_SINGLEVALUEDRASTER
#endif



/*!
  \file
  This file contains the implementation of the SingleValuedRasterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SINGLEVALUEDRASTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::SingleValuedRasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SingleValuedRasterTest> instance(new SingleValuedRasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SingleValuedRasterTest::testThis, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SingleValuedRasterTest::testIRaster, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SINGLEVALUEDRASTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::SingleValuedRasterTest::SingleValuedRasterTest()
{
}



//! setUp
void com::SingleValuedRasterTest::setUp()
{
}

//! tearDown
void com::SingleValuedRasterTest::tearDown()
{
}



void com::SingleValuedRasterTest::testThis()
{
  SingleValuedRaster<UINT1> v(4,5,8);
  BOOST_CHECK(v.nrRows()==4);
  BOOST_CHECK(v.nrCols()==5);
  BOOST_CHECK(v.cell(0,0)==8);
  BOOST_CHECK(v.cell(3,4)==8);

  v.setMV(0,2);

  BOOST_CHECK(v.cell(0,0)==MV_UINT1);
  BOOST_CHECK(v.isMV(0,0));
  BOOST_CHECK(v.isMV(2,0));
}

void com::SingleValuedRasterTest::testIRaster()
{
}
