#ifndef INCLUDED_DAL_MATRIXDRIVERTEST
#include "dal_MatrixDriverTest.h"
#define INCLUDED_DAL_MATRIXDRIVERTEST
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
  This file contains the implementation of the MatrixDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIXDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::MatrixDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MatrixDriverTest> instance(new MatrixDriverTest());

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MATRIXDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::MatrixDriverTest::MatrixDriverTest()
{
}



//! setUp
void dal::MatrixDriverTest::setUp()
{
}



//! tearDown
void dal::MatrixDriverTest::tearDown()
{
}



