#ifndef INCLUDED_DEV_MATHUTILSTEST
#include "dev_MathUtilsTest.h"
#define INCLUDED_DEV_MATHUTILSTEST
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
  This file contains the implementation of the MathUtilsTest class.
*/



namespace dev {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATHUTILSTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* MathUtilsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MathUtilsTest> instance(new MathUtilsTest());
  suite->add(BOOST_CLASS_TEST_CASE(&MathUtilsTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MATHUTILSTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
MathUtilsTest::MathUtilsTest()
{
}



void MathUtilsTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace dev

