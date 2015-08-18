#ifndef INCLUDED_DAL_VTKBLOCKDRIVERTEST
#include "dal_VTKBlockDriverTest.h"
#define INCLUDED_DAL_VTKBLOCKDRIVERTEST
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
#ifndef INCLUDED_DAL_VTKBLOCKDRIVER
#include "dal_VTKBlockDriver.h"
#define INCLUDED_DAL_VTKBLOCKDRIVER
#endif



/*!
  \file
  This file contains the implementation of the VTKBlockDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VTKBLOCKDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*VTKBlockDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VTKBlockDriverTest> instance(new VTKBlockDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VTKBlockDriverTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VTKBLOCKDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
VTKBlockDriverTest::VTKBlockDriverTest(
         )
{
}



//! setUp
void VTKBlockDriverTest::setUp()
{
}



//! tearDown
void VTKBlockDriverTest::tearDown()
{
}



void VTKBlockDriverTest::test()
{
  VTKBlockDriver driver;
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace dal

