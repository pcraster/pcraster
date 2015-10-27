#define BOOST_TEST_MODULE pcraster aguila test
#include <boost/test/unit_test.hpp>
#include "ag_AguilaGuiTest.h"


BOOST_AUTO_TEST_CASE(gui)
{
  using namespace ag;

  AguilaGuiTest test;
  char const* argv[2] = { "AguilaGuiTest", "-silent" };
  BOOST_CHECK_EQUAL(QTest::qExec(&test, 2, const_cast<char**>(argv)), 0);
}
