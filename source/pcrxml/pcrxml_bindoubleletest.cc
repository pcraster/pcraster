#define BOOST_TEST_MODULE pcraster pcrxml bin_double_le
#include <boost/test/unit_test.hpp>
#include "pcrxml_bindoublele.h"

BOOST_AUTO_TEST_CASE(encoding)
{
  using namespace pcrxml;

  {
    BinDoubleLE const v(0);
    BOOST_TEST(v.attrValueStr() == "0000000000000000");
  }
  {
    BinDoubleLE const v(1234.34);
    double const cmpV = v();
    BOOST_TEST(cmpV == BinDoubleLE::hexToDouble(v.attrValueStr()));
  }
  {
    BinDoubleLE const v(0.001);
    double const cmpV = v();
    BOOST_TEST(cmpV == BinDoubleLE::hexToDouble(v.attrValueStr()));
    BOOST_TEST(cmpV == 0.001);
  }
}
