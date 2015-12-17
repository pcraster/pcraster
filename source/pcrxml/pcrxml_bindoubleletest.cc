#define BOOST_TEST_MODULE pcraster pcrxml bin_double_le
#include <boost/test/unit_test.hpp>
#include "pcrxml_bindoublele.h"


BOOST_AUTO_TEST_CASE(encoding)
{
  using namespace pcrxml;

  {
    BinDoubleLE v(0);
    BOOST_CHECK(v.attrValueStr() == "0000000000000000");
  }
  {
    BinDoubleLE v(1234.34);
    double cmpV = v();
    BOOST_CHECK(cmpV == BinDoubleLE::hexToDouble(v.attrValueStr()));
  }
  {
    BinDoubleLE v(0.001);
    double cmpV = v();
    BOOST_CHECK(cmpV == BinDoubleLE::hexToDouble(v.attrValueStr()));
    BOOST_CHECK(cmpV == 0.001);
  }
}
