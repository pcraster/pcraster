#define BOOST_TEST_MODULE pcraster old_calc parser
#include <boost/test/unit_test.hpp>
#include "geo_filecreatetester.h"
#include "com_exception.h"
#include "calc_runscript.h"


BOOST_AUTO_TEST_CASE(model_parser)
{
  using namespace calc;

  geo::FileCreateTester mt("parsertest.res");
  runScriptString("parsertest.res = inp1s.map + 4;");
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
}
