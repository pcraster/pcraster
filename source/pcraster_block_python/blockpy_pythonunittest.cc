#include "dev_IncludePythonApi.h"

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCKPY_PYTHONUNITTEST
#include "blockpy_pythonunittest.h"
#define INCLUDED_BLOCKPY_PYTHONUNITTEST
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
  This file contains the implementation of the PythonUnitTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PYTHONUNIT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*blockpy::PythonUnitTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PythonUnitTest> instance(new PythonUnitTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PythonUnitTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PYTHONUNIT MEMBERS
//------------------------------------------------------------------------------

//! ctor
blockpy::PythonUnitTest::PythonUnitTest()
{
}



//! setUp
void blockpy::PythonUnitTest::setUp()
{
}



//! tearDown
void blockpy::PythonUnitTest::tearDown()
{
}



void blockpy::PythonUnitTest::test()
{
  bool pythonEnvironmentIsConfigured = false;
  BOOST_WARN(pythonEnvironmentIsConfigured);

  // OLS: do not use Py_Main in combination with boost check
  //      return value of Py_Main will not be checked
  //
  // TODO PORT
  // TODO char arg0[15] = "PythonUnitTest";
  // TODO char arg1[8] = "test.py";
  // TODO char *argv[2] = { arg0, arg1 };
  // TODO BOOST_CHECK(Py_Main(2, argv) == 0);
}
