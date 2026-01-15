#define BOOST_TEST_MODULE pcraster model_engine dimensionparser
#include <boost/test/unit_test.hpp>
#include "com_exception.h"

#define private public
#include "calc_dimensionparser.h"

// NOTE use string failureExpected in files expected to fail, see style guide


BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

  {
    DimensionParser dp("a 4");
    BOOST_TEST(dp.d_symbols.size() == 1);
    BOOST_TEST(dp.d_symbols[0].d_symbol == "a");
    BOOST_TEST(dp.d_symbols[0].d_power == 4);
  }
  {
    DimensionParser dp("m2s-1");
    BOOST_TEST(dp.d_symbols.size() == 2);
    BOOST_TEST(dp.d_symbols[0].d_symbol == "m");
    BOOST_TEST(dp.d_symbols[0].d_power == 2);
    BOOST_TEST(dp.d_symbols[1].d_symbol == "s");
    BOOST_TEST(dp.d_symbols[1].d_power == -1);
  }
  {
    DimensionParser dp("kg,m");
    BOOST_TEST(dp.d_symbols.size() == 2);
    BOOST_TEST(dp.d_symbols[0].d_symbol == "kg");
    BOOST_TEST(dp.d_symbols[0].d_power == 1);
    BOOST_TEST(dp.d_symbols[1].d_symbol == "m");
    BOOST_TEST(dp.d_symbols[1].d_power == 1);
  }
}

BOOST_AUTO_TEST_CASE(testError)
{
  using namespace calc;

  try {
    DimensionParser const dp("kg;m");
  } catch (const com::Exception &e) {
    BOOST_TEST(e.messages().find("';' is not a recognized unit dimension") != std::string::npos);
  }
  try {
    DimensionParser const dp("X kg");
  } catch (const com::Exception &e) {
    BOOST_TEST(e.messages().find('X') != std::string::npos);
  }
}
