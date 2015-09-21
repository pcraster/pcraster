#define BOOST_TEST_MODULE pcraster utils
#include <boost/test/unit_test.hpp>
#include "dev_Utils.h"


BOOST_AUTO_TEST_CASE(unique_)
{
  using namespace dev;

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


BOOST_AUTO_TEST_CASE(environment_variable)
{
  using namespace dev;

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
