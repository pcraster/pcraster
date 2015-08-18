#ifndef INCLUDED_DAL_CONSTANTTEST
#include "dal_ConstantTest.h"
#define INCLUDED_DAL_CONSTANTTEST
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
  This file contains the implementation of the ConstantTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CONSTANTTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* ConstantTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ConstantTest> instance(new ConstantTest());
  suite->add(BOOST_CLASS_TEST_CASE(&ConstantTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CONSTANTTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
ConstantTest::ConstantTest()
{
}



void ConstantTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace dal

