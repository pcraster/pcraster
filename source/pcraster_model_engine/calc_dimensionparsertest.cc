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
  BOOST_CHECK(dp.d_symbols.size()==1);
  BOOST_CHECK(dp.d_symbols[0].d_symbol == "a");
  BOOST_CHECK(dp.d_symbols[0].d_power  == 4);
 }
 {
  DimensionParser dp("m2s-1");
  BOOST_CHECK(dp.d_symbols.size()==2);
  BOOST_CHECK(dp.d_symbols[0].d_symbol == "m");
  BOOST_CHECK(dp.d_symbols[0].d_power  == 2);
  BOOST_CHECK(dp.d_symbols[1].d_symbol == "s");
  BOOST_CHECK(dp.d_symbols[1].d_power  == -1);
 }
 {
  DimensionParser dp("kg,m");
  BOOST_CHECK(dp.d_symbols.size()==2);
  BOOST_CHECK(dp.d_symbols[0].d_symbol == "kg");
  BOOST_CHECK(dp.d_symbols[0].d_power  == 1);
  BOOST_CHECK(dp.d_symbols[1].d_symbol == "m");
  BOOST_CHECK(dp.d_symbols[1].d_power  == 1);
 }
}


BOOST_AUTO_TEST_CASE(testError)
{
  using namespace calc;

 try {
  DimensionParser dp("kg;m");
 } catch (const com::Exception& e) {
BOOST_CHECK(e.messages().find("';' is not a recognized unit dimension")
   != std::string::npos);
 }
 try {
  DimensionParser dp("X kg");
 } catch (const com::Exception& e) {
BOOST_CHECK(e.messages().find("X") != std::string::npos);
 }

}
