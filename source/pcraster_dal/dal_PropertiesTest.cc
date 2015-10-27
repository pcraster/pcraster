#define BOOST_TEST_MODULE pcraster dal properties
#include <boost/test/unit_test.hpp>
#include "dal_Properties.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

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
