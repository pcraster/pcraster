#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_NEWCLASSTEST
#include "block_newclasstest.h"
#define INCLUDED_BLOCK_NEWCLASSTEST
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
  This file contains the implementation of the NewClassTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC NEWCLASSTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*NewClassTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<NewClassTest> instance(new NewClassTest());

  suite->add(BOOST_CLASS_TEST_CASE(&NewClassTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF NEWCLASSTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
NewClassTest::NewClassTest(
         )
{
}



//! setUp
void NewClassTest::setUp()
{
}



//! tearDown
void NewClassTest::tearDown()
{
}



void NewClassTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace block

