#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MF_MODFLOWTEST
#include "mf_ModflowTest.h"
#define INCLUDED_MF_MODFLOWTEST
#endif

// Library headers.
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
#include <memory>

/*!
  \file
  This file contains the implementation of the ModflowTest class.
*/


namespace mf
{

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MODFLOWTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite *ModflowTest::suite()
{
  boost::unit_test::test_suite *suite = BOOST_TEST_SUITE(__FILE__);
  std::shared_ptr<ModflowTest> instance(new ModflowTest());
  suite->add(BOOST_CLASS_TEST_CASE(&ModflowTest::test, instance));

  return suite;
}

//------------------------------------------------------------------------------
// DEFINITION OF MODFLOWTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
ModflowTest::ModflowTest()
{
}

void ModflowTest::test()
{
  bool testImplemented = false;
  BOOST_TEST_WARN(testImplemented);
}

}  // namespace mf
