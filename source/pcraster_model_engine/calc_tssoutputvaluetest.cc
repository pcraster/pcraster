#define BOOST_TEST_MODULE pcraster model_engine tssoutputvalue
#include <boost/test/unit_test.hpp>
#include <cstdio>
#include <iomanip>
#include <sstream>
#include "stddefx.h"  // only due to ARRAY_SIZE


BOOST_AUTO_TEST_CASE(testCppStream)
// tests to assure migration from C to C++ stream went wel
{
  {
    // VS_LDD, VS_BOOLEAN
    double const vals[] = {0, 1, 9};
    for (double const val : vals) {
      std::ostringstream os;
      const size_t buf_size = 16;
      char buf[buf_size];
      os << std::setw(4) << val;
      std::snprintf(buf, buf_size, "%4g", val);
      BOOST_CHECK(os.str() == std::string(buf));
    }
  }
  {
    // VS_ORDINAL VS_NOMINAL
    double const vals[] = {0, 1, 9, -12345, 999999};
    for (double const val : vals) {
      std::ostringstream os;
      const size_t buf_size = 16;
      char buf[buf_size];
      os << std::setw(10) << val;
      std::snprintf(buf, buf_size, "%10g", val);
      BOOST_CHECK(os.str() == std::string(buf));
    }
  }
  {
    // VS_SCALAR VS_DIRECTIONAL
    double const vals[] = {0, 1, 9, -12345, 999999, 123.56, 0.00004, -12345689101214.456748};
    for (double const val : vals) {
      std::ostringstream os;
      const size_t buf_size = 16;
      char buf[buf_size];
      os << std::setw(11) << val;
      std::snprintf(buf, buf_size, "%11.6g", val);
      BOOST_CHECK(os.str() == std::string(buf));
    }
  }
}
