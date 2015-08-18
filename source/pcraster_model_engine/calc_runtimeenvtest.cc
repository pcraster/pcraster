#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RUNTIMEENVTEST
#include "calc_runtimeenvtest.h"
#define INCLUDED_CALC_RUNTIMEENVTEST
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
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

/*!
  \file
  This file contains the implementation of the RunTimeEnvTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNTIMEENV MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::RunTimeEnvTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RunTimeEnvTest> instance(new RunTimeEnvTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RunTimeEnvTest::testPopField, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RUNTIMEENV MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::RunTimeEnvTest::RunTimeEnvTest()
{
}



//! setUp
void calc::RunTimeEnvTest::setUp()
{
}

//! tearDown
void calc::RunTimeEnvTest::tearDown()
{
}




void calc::RunTimeEnvTest::testPopField()
{
  RunTimeEnv      rte(geo::RasterSpace(2,2));
  REAL4 zData[4]= { 1,1,1,1};
  Spatial *sp=new Spatial(VS_S,zData,4);

  BOOST_CHECK(!sp->readOnlyReference());

  sp->setReadOnlyReference(true);
  rte.pushField(sp);

  Field *f= rte.popField();
  BOOST_CHECK(f==sp);
  BOOST_CHECK(f->readOnlyReference());

  BOOST_CHECK(f->vs() == VS_S);
  BOOST_CHECK(f->src_f()[0] == 1);
  BOOST_CHECK(f->src_f()[3] == 1);

  deleteAlways(sp);
}
