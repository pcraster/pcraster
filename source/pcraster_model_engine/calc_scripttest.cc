#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_SCRIPTTEST
#include "calc_scripttest.h"
#define INCLUDED_CALC_SCRIPTTEST
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
  This file contains the implementation of the ScriptTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCRIPT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ScriptTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ScriptTest> instance(new ScriptTest());

  // suite->add(BOOST_CLASS_TEST_CASE(&ScriptTest::testResolve, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SCRIPT MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ScriptTest::ScriptTest()
{
}



//! setUp
void calc::ScriptTest::setUp()
{
}



//! tearDown
void calc::ScriptTest::tearDown()
{
}
