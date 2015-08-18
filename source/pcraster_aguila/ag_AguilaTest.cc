#include "ag_AguilaTest.h"

// External headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// Project headers.

// Module headers.
#include "ag_AguilaGuiTest.h"



/*!
  \file
  This file contains the implementation of the AguilaTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC AGUILATEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* AguilaTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AguilaTest> instance(new AguilaTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &AguilaTest::testGui, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF AGUILATEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
AguilaTest::AguilaTest()
{
}



void AguilaTest::testGui()
{
  AguilaGuiTest test;
  char const* argv[2] = { "AguilaGuiTest", "-silent" };
  BOOST_CHECK_EQUAL(QTest::qExec(&test, 2, const_cast<char**>(argv)), 0);
}

} // namespace ag

