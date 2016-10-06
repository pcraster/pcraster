#define BOOST_TEST_MODULE pcraster dal regular_expressions
#include <boost/test/unit_test.hpp>
#include <iostream>
#include "dal_RegularExpressions.h"


BOOST_AUTO_TEST_CASE(quantile_of_raster_regex)
{
  using namespace dal;

  std::smatch match;
  std::string name;

  {
    // Sanity check.
    name = "cd_0.001";
    BOOST_REQUIRE(std::regex_match(name, match, quantileOfRasterRegex));
    BOOST_CHECK(std::string(match[1].first, match[1].second) == "cd");
    BOOST_CHECK(std::string(match[2].first, match[2].second) == "0.001");
  }

  {
    // Embedding an underscore must be possible.
    name = "cd_trend_0.001";
    BOOST_REQUIRE(std::regex_match(name, match, quantileOfRasterRegex));
    BOOST_CHECK(std::string(match[1].first, match[1].second) == "cd_trend");
    BOOST_CHECK(std::string(match[2].first, match[2].second) == "0.001");
  }
}
