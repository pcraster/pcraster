#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_MACKEYBRIDGECOMPACTORTEST
#include "block_mackeybridgecompactortest.h"
#define INCLUDED_BLOCK_MACKEYBRIDGECOMPACTORTEST
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
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.
#ifndef INCLUDED_BLOCK_DUMMYCOMPACTOR
#include "block_dummycompactor.h"
#define INCLUDED_BLOCK_DUMMYCOMPACTOR
#endif

#ifndef INCLUDED_BLOCK_SANDCOMPACTOR
#include "block_sandcompactor.h"
#define INCLUDED_BLOCK_SANDCOMPACTOR
#endif



/*!
  \file
  This file contains the implementation of the MackeyBridgeCompactorTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MACKEYBRIDGECOMPACTORTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*MackeyBridgeCompactorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MackeyBridgeCompactorTest> instance(new MackeyBridgeCompactorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MackeyBridgeCompactorTest::testDummyCompactor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MackeyBridgeCompactorTest::testSandCompactor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MackeyBridgeCompactorTest::testClayCompactor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MACKEYBRIDGECOMPACTORTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
MackeyBridgeCompactorTest::MackeyBridgeCompactorTest(
         )
{
}



//! setUp
void MackeyBridgeCompactorTest::setUp()
{
}



//! tearDown
void MackeyBridgeCompactorTest::tearDown()
{
}



void MackeyBridgeCompactorTest::testDummyCompactor()
{
  DummyCompactor compactor;
  BOOST_CHECK(dal::comparable(compactor( 0.0,  0.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 5.0,  0.0), REAL4(5.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, -5.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0,  5.0), REAL4(0.0)));
}



void MackeyBridgeCompactorTest::testSandCompactor()
{
  SandCompactor compactor;
  BOOST_CHECK(dal::comparable(compactor( 0.0,   0.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
}



void MackeyBridgeCompactorTest::testClayCompactor()
{
  SandCompactor compactor;
  BOOST_CHECK(dal::comparable(compactor( 0.0,   0.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
}

} // namespace block

