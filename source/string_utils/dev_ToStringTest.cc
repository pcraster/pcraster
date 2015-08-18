#ifndef INCLUDED_DEV_TOSTRINGTEST
#include "dev_ToStringTest.h"
#define INCLUDED_DEV_TOSTRINGTEST
#endif

// External headers.
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

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ToStringTest class.
*/



namespace dev {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TOSTRINGTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* ToStringTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ToStringTest> instance(new ToStringTest());
  suite->add(BOOST_CLASS_TEST_CASE(&ToStringTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TOSTRINGTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
ToStringTest::ToStringTest()
{
}



void ToStringTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace dev

