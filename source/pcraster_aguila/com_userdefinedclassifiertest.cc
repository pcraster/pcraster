#include "com_userdefinedclassifiertest.h"

// Library headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the UserDefinedClassifierTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace com {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC USERDEFINEDCLASSIFIER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*UserDefinedClassifierTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<UserDefinedClassifierTest> instance(new UserDefinedClassifierTest());

  suite->add(BOOST_CLASS_TEST_CASE(&UserDefinedClassifierTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF USERDEFINEDCLASSIFIER MEMBERS
//------------------------------------------------------------------------------

//! ctor
UserDefinedClassifierTest::UserDefinedClassifierTest(
         )
{
}



//! setUp
void UserDefinedClassifierTest::setUp()
{
}



//! tearDown
void UserDefinedClassifierTest::tearDown()
{
}



void UserDefinedClassifierTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



} // namespace com

