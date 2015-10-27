#define BOOST_TEST_MODULE pcraster dal def
#include <boost/test/unit_test.hpp>
#include <limits>
#include "dal_Def.h"


BOOST_AUTO_TEST_CASE(type_sizes)
{
  using namespace dal;

  BOOST_CHECK_EQUAL(sizeof(UINT1), size_t(1));
  BOOST_CHECK_EQUAL(sizeof(UINT2), size_t(2));
  BOOST_CHECK_EQUAL(sizeof(UINT4), size_t(4));
  BOOST_CHECK_EQUAL(sizeof(PCR_UINT8), size_t(8));

  BOOST_CHECK_EQUAL(MV_UINT1, std::numeric_limits<UINT1>::max());
  BOOST_CHECK_EQUAL(MV_UINT2, std::numeric_limits<UINT2>::max());
  BOOST_CHECK_EQUAL(MV_UINT4, std::numeric_limits<UINT4>::max());
  BOOST_CHECK_EQUAL(MV_UINT8, std::numeric_limits<PCR_UINT8>::max());

  BOOST_CHECK_EQUAL(sizeof(INT1), size_t(1));
  BOOST_CHECK_EQUAL(sizeof(INT2), size_t(2));
  BOOST_CHECK_EQUAL(sizeof(INT4), size_t(4));
  BOOST_CHECK_EQUAL(sizeof(PCR_INT8), size_t(8));

  BOOST_CHECK_EQUAL(MV_INT1, std::numeric_limits<INT1>::min());
  BOOST_CHECK_EQUAL(MV_INT2, std::numeric_limits<INT2>::min());
  BOOST_CHECK_EQUAL(MV_INT4, std::numeric_limits<INT4>::min());
  BOOST_CHECK_EQUAL(MV_INT8, std::numeric_limits<PCR_INT8>::min());

  BOOST_CHECK_EQUAL(sizeof(REAL4), size_t(4));
  BOOST_CHECK_EQUAL(sizeof(REAL8), size_t(8));

  {
    PCR_INT8 v=1;
    // test the non type safe macros that never should be used in the code
    BOOST_CHECK(! IS_MV_INT8(&v));
    SET_MV_INT8(&v);
    BOOST_CHECK_EQUAL(v,MV_INT8);
    BOOST_CHECK(IS_MV_INT8(&v));
  }
  {
    PCR_UINT8 v=1;
    // test the non type safe macros that never should be used in the code
    BOOST_CHECK(! IS_MV_UINT8(&v));
    SET_MV_UINT8(&v);
    BOOST_CHECK_EQUAL(v,MV_UINT8);
    BOOST_CHECK(IS_MV_UINT8(&v));
  }

  {
    PCR_INT8 v=1;
    // test the type safe templates
    BOOST_CHECK(! pcr::isMV(v));
    pcr::setMV(v);
    BOOST_CHECK_EQUAL(v,MV_INT8);
    BOOST_CHECK(pcr::isMV(v));
  }
  {
    PCR_UINT8 v=1;
    // test the type safe templates
    BOOST_CHECK(! pcr::isMV(v));
    pcr::setMV(v);
    BOOST_CHECK_EQUAL(v,MV_UINT8);
    BOOST_CHECK(pcr::isMV(v));
  }
}
