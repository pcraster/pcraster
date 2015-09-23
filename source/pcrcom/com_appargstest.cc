#define BOOST_TEST_MODULE pcraster com app_args
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_appargs.h"
#include <iostream>
#include <stdexcept>


BOOST_AUTO_TEST_CASE(argv_argc)
{
 {
  com::AppArgs in1("no whe at end");
  char **argv=in1.argv();
  BOOST_CHECK(in1.argc()==4);
  BOOST_CHECK(argv[0] == std::string("no"));
  BOOST_CHECK(argv[1] == std::string("whe"));
  BOOST_CHECK(argv[2] == std::string("at"));
  BOOST_CHECK(argv[3] == std::string("end"));
 }
 {
  com::AppArgs in1("no"," whe at end");
  char **argv=in1.argv();
  BOOST_CHECK(in1.argc()==4);
  BOOST_CHECK(argv[0] == std::string("no"));
  BOOST_CHECK(argv[1] == std::string("whe"));
  BOOST_CHECK(argv[2] == std::string("at"));
  BOOST_CHECK(argv[3] == std::string("end"));
 }
}
