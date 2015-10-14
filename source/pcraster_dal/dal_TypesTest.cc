#define BOOST_TEST_MODULE pcraster dal types
#include <boost/test/unit_test.hpp>
#include "dal_Types.h"


BOOST_AUTO_TEST_CASE(id_of_smallest_type)
{
  using namespace dal;

  {
    // unsigned integers:
    // 1 byte  2^8  = 0 -        255
    // 2 bytes 2^16 = 0 -      65535
    // 4 bytes 2^32 = 0 - 4294967295

    Types types;
    BOOST_CHECK_EQUAL(types.idOfSmallestType(         "0"), TI_UINT1);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(         "1"), TI_UINT1);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(       "255"), TI_UINT1);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(       "256"), TI_UINT2);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(     "65535"), TI_UINT2);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(     "65536"), TI_UINT4);
    BOOST_CHECK_EQUAL(types.idOfSmallestType("4294967295"), TI_UINT4);
    BOOST_CHECK_EQUAL(types.idOfSmallestType("4294967296"), TI_REAL4);

    BOOST_CHECK_EQUAL(types.idOfSmallestType(         "-1"), TI_INT1);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(       "-128"), TI_INT1);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(       "-129"), TI_INT2);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(     "-32768"), TI_INT2);
    BOOST_CHECK_EQUAL(types.idOfSmallestType(     "-32769"), TI_INT4);
    BOOST_CHECK_EQUAL(types.idOfSmallestType("-2147483648"), TI_INT4);
    BOOST_CHECK_EQUAL(types.idOfSmallestType("-2147483649"), TI_REAL4);

    // Overgang REAL4 / REAL8...
    // String...

    BOOST_WARN_EQUAL(types.idOfSmallestType("NROWS"), TI_STRING);
  }
}


BOOST_AUTO_TEST_CASE(id_of_largest_type)
{
  using namespace dal;

  Types types;
  BOOST_CHECK_EQUAL(types.idOfLargestType(TI_UINT1, TI_UINT2), TI_UINT2);

  bool idOfLargestTypeImplemented = false;
  BOOST_WARN(idOfLargestTypeImplemented);
}
