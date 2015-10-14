#define BOOST_TEST_MODULE pcraster dal type
#include <boost/test/unit_test.hpp>
#include "dal_Type.h"
#include "dal_BasicTypes.h"


BOOST_AUTO_TEST_CASE(id)
{
  using namespace dal;

  {
    Uint1Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_UINT1);
  }

  {
    Uint2Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_UINT2);
  }

  {
    Uint4Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_UINT4);
  }

  {
    Int1Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_INT1);
  }

  {
    Int2Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_INT2);
  }

  {
    Int4Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_INT4);
  }

  {
    Real4Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_REAL4);
  }

  {
    Real8Type type;
    BOOST_CHECK_EQUAL(type.id(), TI_REAL8);
  }

  /*
* BOOST_CHECK_EQUAL(TypeTraits<UINT1>::id, TI_UINT1);
* BOOST_CHECK_EQUAL(TypeTraits<UINT2>::id, TI_UINT2);
* BOOST_CHECK_EQUAL(TypeTraits<UINT4>::id, TI_UINT4);
* BOOST_CHECK_EQUAL(TypeTraits<INT1>::id, TI_INT1);
* BOOST_CHECK_EQUAL(TypeTraits<INT2>::id, TI_INT2);
* BOOST_CHECK_EQUAL(TypeTraits<INT4>::id, TI_INT4);
* BOOST_CHECK_EQUAL(TypeTraits<REAL4>::id, TI_REAL4);
* BOOST_CHECK_EQUAL(TypeTraits<REAL8>::id, TI_REAL8);
* BOOST_CHECK_EQUAL(TypeTraits<std::string>::id, TI_STRING);
  */
}


BOOST_AUTO_TEST_CASE(can_parse_uint1)
{
  using namespace dal;

  Uint1Type type;

  BOOST_CHECK(!type.canParse("-1"));
  BOOST_CHECK(type.canParse("0"));
  BOOST_CHECK(type.canParse("1"));
  BOOST_CHECK(type.canParse("255"));
  BOOST_CHECK(!type.canParse("256"));
  BOOST_CHECK(!type.canParse("257"));
  BOOST_CHECK(!type.canParse("258"));
  // bool TODOUint1CannotParse999 = false;
  // BOOST_CHECK(TODOUint1CannotParse999);
  // BOOST_CHECK(!type.canParse("999"));
  BOOST_CHECK(!type.canParse("1000"));
  BOOST_CHECK(!type.canParse("10000"));
  BOOST_CHECK(!type.canParse("50000"));
  BOOST_CHECK(!type.canParse("65534"));
  BOOST_CHECK(!type.canParse("65535"));
  BOOST_CHECK(!type.canParse("65536"));

  BOOST_CHECK(!type.canParse("1.0"));
  BOOST_CHECK(!type.canParse("x"));
  BOOST_CHECK(!type.canParse("1x"));
  BOOST_CHECK(!type.canParse("x1"));


/*
* BOOST_CHECK_EQUAL(TypeTraits<UINT1>::id, TI_UINT1);
* BOOST_CHECK(TypeTraits<UINT1>::canParse("0"));
* BOOST_CHECK(TypeTraits<UINT1>::canParse("1"));
* BOOST_CHECK(TypeTraits<UINT1>::canParse("255"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("256"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("257"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("258"));
* {
*   bool TODOUint1CannotParse999 = false;
*   BOOST_CHECK(TODOUint1CannotParse999);
* }
* // BOOST_CHECK(!TypeTraits<UINT1>::canParse("999"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("1000"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("10000"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("50000"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("65534"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("65535"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("65536"));
*
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("1.0"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("x"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("1x"));
* BOOST_CHECK(!TypeTraits<UINT1>::canParse("x1"));
* */
}


BOOST_AUTO_TEST_CASE(can_parse_uint2)
{
  using namespace dal;

  Uint2Type type;

  BOOST_CHECK(!type.canParse("-1"));
  BOOST_CHECK(type.canParse("0"));
  BOOST_CHECK(type.canParse("1"));
  BOOST_CHECK(type.canParse("65535"));
  BOOST_CHECK(!type.canParse("65536"));

  BOOST_CHECK(!type.canParse("1.0"));
  BOOST_CHECK(!type.canParse("x"));
  BOOST_CHECK(!type.canParse("1x"));
  BOOST_CHECK(!type.canParse("x1"));
}


BOOST_AUTO_TEST_CASE(can_parse_uint4)
{
  using namespace dal;

  Uint4Type type;

  BOOST_CHECK(!type.canParse("-1"));
  BOOST_CHECK(type.canParse("0"));
  BOOST_CHECK(type.canParse("1"));
  BOOST_CHECK(type.canParse("65535"));
  BOOST_CHECK(type.canParse("65536"));
  BOOST_CHECK(type.canParse("4294967295"));
  BOOST_CHECK(!type.canParse("4294967296"));

  BOOST_CHECK(!type.canParse("1.0"));
  BOOST_CHECK(!type.canParse("x"));
  BOOST_CHECK(!type.canParse("1x"));
  BOOST_CHECK(!type.canParse("x1"));
}


BOOST_AUTO_TEST_CASE(can_parse_int1)
{
  using namespace dal;

  Int1Type type;

  BOOST_CHECK(!type.canParse("-129"));
  BOOST_CHECK(type.canParse("-128"));
  BOOST_CHECK(type.canParse("-1"));
  BOOST_CHECK(!type.canParse("-1.0"));
  BOOST_CHECK(type.canParse("0"));
  BOOST_CHECK(!type.canParse("0.0"));
  BOOST_CHECK(!type.canParse(".0"));
  BOOST_CHECK(!type.canParse("0."));
  BOOST_CHECK(type.canParse("1"));
  BOOST_CHECK(!type.canParse("1.0"));
  BOOST_CHECK(!type.canParse(".1"));
  BOOST_CHECK(type.canParse("127"));
  BOOST_CHECK(!type.canParse("128"));
}


BOOST_AUTO_TEST_CASE(can_parse_int2)
{
  using namespace dal;

  Int2Type type;

  BOOST_CHECK(!type.canParse("-32769"));
  BOOST_CHECK(type.canParse("-32768"));
  BOOST_CHECK(type.canParse("-1"));
  BOOST_CHECK(!type.canParse("-1.0"));
  BOOST_CHECK(type.canParse("0"));
  BOOST_CHECK(!type.canParse("0.0"));
  BOOST_CHECK(!type.canParse(".0"));
  BOOST_CHECK(!type.canParse("0."));
  BOOST_CHECK(type.canParse("1"));
  BOOST_CHECK(!type.canParse("1.0"));
  BOOST_CHECK(!type.canParse(".1"));
  BOOST_CHECK(type.canParse("32767"));
  BOOST_CHECK(!type.canParse("32768"));
}


BOOST_AUTO_TEST_CASE(can_parse_int4)
{
  using namespace dal;

  Int4Type type;

  BOOST_CHECK(!type.canParse("-2147483649"));
  BOOST_CHECK(type.canParse("-2147483648"));
  BOOST_CHECK(type.canParse("-1"));
  BOOST_CHECK(!type.canParse("-1.0"));
  BOOST_CHECK(type.canParse("0"));
  BOOST_CHECK(!type.canParse("0.0"));
  BOOST_CHECK(!type.canParse(".0"));
  BOOST_CHECK(!type.canParse("0."));
  BOOST_CHECK(type.canParse("1"));
  BOOST_CHECK(!type.canParse("1.0"));
  BOOST_CHECK(!type.canParse(".1"));
  BOOST_CHECK(type.canParse("2147483647"));
  BOOST_CHECK(!type.canParse("2147483648"));
}


BOOST_AUTO_TEST_CASE(can_parse_real4)
{
  using namespace dal;

  Real4Type type;

  BOOST_CHECK(type.canParse("5.0"));
  BOOST_CHECK(type.canParse("5"));
  BOOST_CHECK(type.canParse(".5"));
  BOOST_CHECK(type.canParse("5."));
  BOOST_CHECK(type.canParse("-5"));

  BOOST_CHECK(!type.canParse(""));
  BOOST_CHECK(!type.canParse("a"));
  BOOST_CHECK(!type.canParse("5.0a"));
  BOOST_CHECK(!type.canParse("a5.0"));
  BOOST_CHECK(!type.canParse("5.."));
  BOOST_CHECK(!type.canParse("..5"));
  BOOST_CHECK(!type.canParse("5..0"));
  BOOST_CHECK(!type.canParse("5.0."));
  BOOST_CHECK(!type.canParse("5.0.2"));

  BOOST_CHECK(!type.canParse("5,0"));
  BOOST_CHECK(!type.canParse(",0"));
  BOOST_CHECK(!type.canParse("5,"));
}


BOOST_AUTO_TEST_CASE(can_parse_real8)
{
  using namespace dal;

  Real8Type type;

  BOOST_CHECK(type.canParse("5.0"));
  BOOST_CHECK(type.canParse("5"));
  BOOST_CHECK(type.canParse(".5"));
  BOOST_CHECK(type.canParse("5."));

  BOOST_CHECK(!type.canParse(""));
  BOOST_CHECK(!type.canParse("a"));
  BOOST_CHECK(!type.canParse("5.0a"));
  BOOST_CHECK(!type.canParse("a5.0"));
  BOOST_CHECK(!type.canParse("5.."));
  BOOST_CHECK(!type.canParse("..5"));
  BOOST_CHECK(!type.canParse("5..0"));
  BOOST_CHECK(!type.canParse("5.0."));
  BOOST_CHECK(!type.canParse("5.0.2"));

  BOOST_CHECK(!type.canParse("5,0"));
  BOOST_CHECK(!type.canParse(",0"));
  BOOST_CHECK(!type.canParse("5,"));
}


BOOST_AUTO_TEST_CASE(type_traits)
{
  using namespace dal;

  BOOST_CHECK_EQUAL(TypeTraits<UINT1>::typeId, TI_UINT1);
  BOOST_CHECK_EQUAL(TypeTraits<UINT1>::csfCr, CR_UINT1);
  BOOST_CHECK(TypeTraits<std::string>::typeId != TI_UINT1);
  BOOST_CHECK_EQUAL(TypeTraits<std::string>::typeId, TI_STRING);

  TypeOfTypeId<TI_UINT1> v;
  BOOST_CHECK_EQUAL(sizeof(v), size_t(1));
}


BOOST_AUTO_TEST_CASE(basic_type)
{
  using namespace dal;

  Type const& i2(Type::get(TI_INT2));
  BOOST_CHECK_EQUAL(i2.size(), size_t(2));
  BOOST_CHECK_EQUAL(i2.id(), TI_INT2);
  BOOST_CHECK(i2.hasTrivialCopy());
}
