#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FUNC_BOOLEANOPERATORSTEST
#include "func_booleanoperatorstest.h"
#define INCLUDED_FUNC_BOOLEANOPERATORSTEST
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



/*!
  \file
  This file contains the implementation of the BooleanOperatorsTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace func {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BOOLEANOPERATORSTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*BooleanOperatorsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BooleanOperatorsTest> instance(new BooleanOperatorsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BooleanOperatorsTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BOOLEANOPERATORSTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
BooleanOperatorsTest::BooleanOperatorsTest(
         )
{
}



//! setUp
void BooleanOperatorsTest::setUp()
{
}



//! tearDown
void BooleanOperatorsTest::tearDown()
{
}



void BooleanOperatorsTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace func

