#ifndef INCLUDED_DAL_BINARYTABLEDRIVERTEST
#include "dal_BinaryTableDriverTest.h"
#define INCLUDED_DAL_BINARYTABLEDRIVERTEST
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
  This file contains the implementation of the BinaryTableDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINARYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*BinaryTableDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BinaryTableDriverTest> instance(new BinaryTableDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BinaryTableDriverTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BINARYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
BinaryTableDriverTest::BinaryTableDriverTest(
         )
{
}



//! setUp
void BinaryTableDriverTest::setUp()
{
}



//! tearDown
void BinaryTableDriverTest::tearDown()
{
}



void BinaryTableDriverTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



} // namespace dal

