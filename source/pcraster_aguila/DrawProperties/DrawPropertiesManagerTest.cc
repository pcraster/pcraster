#ifndef INCLUDED_DRAWPROPERTIESMANAGERTEST
#include "DrawPropertiesManagerTest.h"
#define INCLUDED_DRAWPROPERTIESMANAGERTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DRAWPROPERTIES
#include "DrawProperties.h"
#define INCLUDED_DRAWPROPERTIES
#endif

#ifndef INCLUDED_DRAWPROPERTIESMANAGER
#include "DrawPropertiesManager.h"
#define INCLUDED_DRAWPROPERTIESMANAGER
#endif



/*!
  \file
  This file contains the implementation of the DrawPropertiesManagerTest class.
*/


namespace {

enum ClassificationCategory {
  ByAttribute,
  ByChances,
  ByClassifiedChances
};

} // Anonymous namespace.



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DRAWPROPERTIESMANAGERTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* DrawPropertiesManagerTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DrawPropertiesManagerTest> instance(
         new DrawPropertiesManagerTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &DrawPropertiesManagerTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DRAWPROPERTIESMANAGERTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
DrawPropertiesManagerTest::DrawPropertiesManagerTest()
{
}



void DrawPropertiesManagerTest::test()
{
  {
    DrawPropertiesManager<int, ClassificationCategory> manager;
    BOOST_CHECK_EQUAL(manager.size(), 0u);

    manager[0][ByAttribute] = boost::shared_ptr<DrawProperties>(
         new DrawProperties());
  }
}

} // namespace ag

