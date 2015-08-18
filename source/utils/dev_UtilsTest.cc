#ifndef INCLUDED_DEV_UTILSTEST
#include "dev_UtilsTest.h"
#define INCLUDED_DEV_UTILSTEST
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
#ifndef INCLUDED_DEV_UTILS
#include "dev_Utils.h"
#define INCLUDED_DEV_UTILS
#endif



/*!
  \file
  This file contains the implementation of the UtilsTest class.
*/



namespace dev {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC UTILSTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* UtilsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<UtilsTest> instance(
         new UtilsTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &UtilsTest::testUnique, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &UtilsTest::testEnvironmentVariables, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF UTILSTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
UtilsTest::UtilsTest()
{
}



void UtilsTest::testUnique()
{
  {
    std::vector<int> container;
    unique(container);
    BOOST_CHECK(container.empty());
  }

  {
    std::vector<int> container;
    container.push_back(6);
    unique(container);
    BOOST_CHECK_EQUAL(container.size(), size_t(1));
    BOOST_CHECK_EQUAL(container[0], 6);
  }

  {
    std::vector<int> container;
    container.push_back(6);
    container.push_back(5);
    unique(container);
    BOOST_CHECK_EQUAL(container.size(), size_t(2));
    BOOST_CHECK_EQUAL(container[0], 6);
    BOOST_CHECK_EQUAL(container[1], 5);
  }

  {
    std::vector<int> container;
    container.push_back(6);
    container.push_back(5);
    container.push_back(6);
    unique(container);
    BOOST_CHECK_EQUAL(container.size(), size_t(2));
    BOOST_CHECK_EQUAL(container[0], 6);
    BOOST_CHECK_EQUAL(container[1], 5);
  }
}



void UtilsTest::testEnvironmentVariables()
{
  BOOST_REQUIRE(!environmentVariableSet("BLA"));

 #ifdef _WIN32
  BOOST_WARN_MESSAGE(false, "Environment handling does not work on windows(?)");
#else
  // Unset non-existing variable.
  unsetEnvironmentVariable("BLA");
  BOOST_CHECK(!environmentVariableSet("BLA"));

  // Create variable.
  setEnvironmentVariable("BLA", "Bla value");
  BOOST_CHECK(environmentVariableSet("BLA"));
  BOOST_CHECK_EQUAL(environmentVariable("BLA"), "Bla value");

  // Update variable to empty string.
  setEnvironmentVariable("BLA", "");
  BOOST_CHECK(environmentVariableSet("BLA"));
  BOOST_CHECK_EQUAL(environmentVariable("BLA"), "");

  // Update variable to non-empty string.
  setEnvironmentVariable("BLA", "Bla value2");
  BOOST_CHECK(environmentVariableSet("BLA"));
  BOOST_CHECK_EQUAL(environmentVariable("BLA"), "Bla value2");

  // Unset existing variable.
  unsetEnvironmentVariable("BLA");
  BOOST_CHECK(!environmentVariableSet("BLA"));
#endif
}

} // namespace dev

