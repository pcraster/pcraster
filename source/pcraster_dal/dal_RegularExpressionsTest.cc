#ifndef INCLUDED_DAL_REGULAREXPRESSIONSTEST
#include "dal_RegularExpressionsTest.h"
#define INCLUDED_REGULAREXPRESSIONSTEST
#endif

// External headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

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
#ifndef INCLUDED_DAL_REGULAREXPRESSIONS
#include "dal_RegularExpressions.h"
#define INCLUDED_DAL_REGULAREXPRESSIONS
#endif



/*!
  \file
  This file contains the implementation of the RegularExpressionsTest class.
*/

namespace {

} // Anonymous namespace



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC REGULAREXPRESSIONSTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* RegularExpressionsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RegularExpressionsTest> instance(
         new RegularExpressionsTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &RegularExpressionsTest::testQuantileOfRasterRegex, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF REGULAREXPRESSIONSTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
RegularExpressionsTest::RegularExpressionsTest()
{
}



void RegularExpressionsTest::testQuantileOfRasterRegex()
{
  boost::smatch match;
  std::string name;

  {
    // Sanity check.
    name = "cd_0.001";
    BOOST_REQUIRE(boost::regex_match(name, match, quantileOfRasterRegex));
    BOOST_CHECK(std::string(match[1].first, match[1].second) == "cd");
    BOOST_CHECK(std::string(match[2].first, match[2].second) == "0.001");
  }

  {
    // Embedding an underscore must be possible.
    name = "cd_trend_0.001";
    BOOST_REQUIRE(boost::regex_match(name, match, quantileOfRasterRegex));
    BOOST_CHECK(std::string(match[1].first, match[1].second) == "cd_trend");
    BOOST_CHECK(std::string(match[2].first, match[2].second) == "0.001");
  }
}

} // namespace dal

