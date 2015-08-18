#ifndef INCLUDED_DAL_TABLEDRIVERTEST
#include "dal_TableDriverTest.h"
#define INCLUDED_DAL_TABLEDRIVERTEST
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
#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif



/*!
  \file
  This file contains the implementation of the TableDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::TableDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TableDriverTest> instance(new TableDriverTest());
  // suite->add(BOOST_CLASS_TEST_CASE(&TableDriverTest::testParseName, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::TableDriverTest::TableDriverTest()
{
}



//! setUp
void dal::TableDriverTest::setUp()
{
}



//! tearDown
void dal::TableDriverTest::tearDown()
{
}



