#ifndef INCLUDED_DAL_BLOCKTEST
#include "dal_BlockTest.h"
#define INCLUDED_DAL_BLOCKTEST
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
  This file contains the implementation of the BlockTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCK MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*BlockTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BlockTest> instance(new BlockTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BLOCK MEMBERS
//------------------------------------------------------------------------------

//! ctor
BlockTest::BlockTest(
         )
{
}



//! setUp
void BlockTest::setUp()
{
}



//! tearDown
void BlockTest::tearDown()
{
}



void BlockTest::test()
{
  bool testImplemented = false;
  BOOST_CHECK(testImplemented);
}



} // namespace dal

