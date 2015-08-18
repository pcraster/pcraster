#ifndef INCLUDED_DAL_PROPERTIESTEST
#include "dal_PropertiesTest.h"
#define INCLUDED_DAL_PROPERTIESTEST
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
#ifndef INCLUDED_DAL_PROPERTIES
#include "dal_Properties.h"
#define INCLUDED_DAL_PROPERTIES
#endif



/*!
  \file
  This file contains the implementation of the PropertiesTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROPERTIES MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*PropertiesTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PropertiesTest> instance(new PropertiesTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PropertiesTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PROPERTIES MEMBERS
//------------------------------------------------------------------------------

//! ctor
PropertiesTest::PropertiesTest(
         )
{
}



//! setUp
void PropertiesTest::setUp()
{
}



//! tearDown
void PropertiesTest::tearDown()
{
}



void PropertiesTest::test()
{
  Properties properties;
  BOOST_CHECK_EQUAL(properties.size(), size_t(0));
  BOOST_CHECK(!properties.hasValue("string"));

  properties.setValue<std::string>("string", "Zaphod");
  BOOST_CHECK_EQUAL(properties.size(), size_t(1));
  BOOST_CHECK(properties.hasValue("string"));
  BOOST_CHECK_EQUAL(properties.value<std::string>("string"), "Zaphod");

  // Overwrite value using setValue().
  properties.setValue<std::string>("string", "Beebel");
  BOOST_CHECK_EQUAL(properties.size(), size_t(1));
  BOOST_CHECK(properties.hasValue("string"));
  BOOST_CHECK_EQUAL(properties.value<std::string>("string"), "Beebel");

  properties.setValue<int>("integer", 5);
  BOOST_CHECK_EQUAL(properties.size(), size_t(2));
  BOOST_CHECK(properties.hasValue("integer"));
  BOOST_CHECK_EQUAL(properties.value<int>("integer"), 5);

  // Overwrite value using value().
  properties.value<int>("integer") = 3;
  BOOST_CHECK_EQUAL(properties.size(), size_t(2));
  BOOST_CHECK(properties.hasValue("integer"));
  BOOST_CHECK_EQUAL(properties.value<int>("integer"), 3);

  BOOST_CHECK_EQUAL(properties.value<int>("notThere",42), 42);

  {
    typedef std::map<std::string, std::pair<int, double> > MapMap;
    MapMap src;
    src["jan"] = std::make_pair(42, 3.4);
    Properties properties;
    properties.setValue<MapMap>("map_map", src);
    BOOST_CHECK_EQUAL(properties.size(), size_t(1));
    BOOST_CHECK(properties.hasValue("map_map"));
    MapMap dest = properties.value<MapMap>("map_map");
    BOOST_CHECK_EQUAL(dest.size(), size_t(1));
  }
}



} // namespace dal

