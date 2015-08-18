#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FUNC_SETMVTEST
#include "func_setmvtest.h"
#define INCLUDED_FUNC_SETMVTEST
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
  This file contains the implementation of the SetMVTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace func {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SETMVTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*SetMVTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SetMVTest> instance(new SetMVTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SetMVTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SETMVTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
SetMVTest::SetMVTest(
         )
{
}



//! setUp
void SetMVTest::setUp()
{
}



//! tearDown
void SetMVTest::tearDown()
{
}



void SetMVTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace func

