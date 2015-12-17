#define BOOST_TEST_MODULE pcraster old_calc tss_output_value
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <iomanip>
#include <sstream>


// tests to assure migration from C to C++ stream went wel
BOOST_AUTO_TEST_CASE(cpp_stream)
{
  {
  // VS_LDD, VS_BOOLEAN
  double vals[] = { 0, 1, 9 };
  for (size_t i =0; i < ARRAY_SIZE(vals); ++i) {
    std::ostringstream os;
    char buf[16];
    os << std::setw(4) << vals[i];
    sprintf(buf,"%4g",vals[i]);
    BOOST_CHECK(os.str() == std::string(buf));
  }
  }
  {
  // VS_ORDINAL VS_NOMINAL
  double vals[] = { 0, 1, 9,-12345,999999 };
  for (size_t i =0; i < ARRAY_SIZE(vals); ++i) {
    std::ostringstream os;
    char buf[16];
    os << std::setw(10) << vals[i];
    sprintf(buf,"%10g",vals[i]);
    BOOST_CHECK(os.str() == std::string(buf));
  }
  }
  {
  // VS_SCALAR VS_DIRECTIONAL
  double vals[] = { 0, 1, 9,-12345,999999,123.56,0.00004,-12345689101214.456748 };
  for (size_t i =0; i < ARRAY_SIZE(vals); ++i) {
    std::ostringstream os;
    char buf[16];
    os << std::setw(11) << vals[i];
    sprintf(buf,"%11.6g",vals[i]);
    BOOST_CHECK(os.str() == std::string(buf));
  }
  }
}
