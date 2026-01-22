#define BOOST_TEST_MODULE pcraster dal type
#include <boost/test/unit_test.hpp>
#include "dal_Type.h"
#include "dal_BasicTypes.h"


BOOST_AUTO_TEST_CASE(id)
{
  using namespace dal;

  {
    Uint1Type const type;
    BOOST_TEST(type.id() == TI_UINT1);
  }

  {
    Uint2Type const type;
    BOOST_TEST(type.id() == TI_UINT2);
  }

  {
    Uint4Type const type;
    BOOST_TEST(type.id() == TI_UINT4);
  }

  {
    Int1Type const type;
    BOOST_TEST(type.id() == TI_INT1);
  }

  {
    Int2Type const type;
    BOOST_TEST(type.id() == TI_INT2);
  }

  {
    Int4Type const type;
    BOOST_TEST(type.id() == TI_INT4);
  }

  {
    Real4Type const type;
    BOOST_TEST(type.id() == TI_REAL4);
  }

  {
    Real8Type const type;
    BOOST_TEST(type.id() == TI_REAL8);
  }

  /*
* BOOST_TEST(TypeTraits<UINT1>::id == TI_UINT1);
* BOOST_TEST(TypeTraits<UINT2>::id == TI_UINT2);
* BOOST_TEST(TypeTraits<UINT4>::id == TI_UINT4);
* BOOST_TEST(TypeTraits<INT1>::id == TI_INT1);
* BOOST_TEST(TypeTraits<INT2>::id == TI_INT2);
* BOOST_TEST(TypeTraits<INT4>::id == TI_INT4);
* BOOST_TEST(TypeTraits<REAL4>::id == TI_REAL4);
* BOOST_TEST(TypeTraits<REAL8>::id == TI_REAL8);
* BOOST_TEST(TypeTraits<std::string>::id == TI_STRING);
  */
}


BOOST_AUTO_TEST_CASE(can_parse_uint1)
{
  using namespace dal;

  Uint1Type const type;

  BOOST_TEST(!type.canParse("-1"));
  BOOST_TEST(type.canParse("0"));
  BOOST_TEST(type.canParse("1"));
  BOOST_TEST(type.canParse("255"));
  BOOST_TEST(!type.canParse("256"));
  BOOST_TEST(!type.canParse("257"));
  BOOST_TEST(!type.canParse("258"));
  // bool TODOUint1CannotParse999 = false;
  // BOOST_TEST(TODOUint1CannotParse999);
  // BOOST_TEST(!type.canParse("999"));
  BOOST_TEST(!type.canParse("1000"));
  BOOST_TEST(!type.canParse("10000"));
  BOOST_TEST(!type.canParse("50000"));
  BOOST_TEST(!type.canParse("65534"));
  BOOST_TEST(!type.canParse("65535"));
  BOOST_TEST(!type.canParse("65536"));

  BOOST_TEST(!type.canParse("1.0"));
  BOOST_TEST(!type.canParse("x"));
  BOOST_TEST(!type.canParse("1x"));
  BOOST_TEST(!type.canParse("x1"));


/*
* BOOST_TEST(TypeTraits<UINT1>::id == TI_UINT1);
* BOOST_TEST(TypeTraits<UINT1>::canParse("0"));
* BOOST_TEST(TypeTraits<UINT1>::canParse("1"));
* BOOST_TEST(TypeTraits<UINT1>::canParse("255"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("256"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("257"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("258"));
* {
*   bool TODOUint1CannotParse999 = false;
*   BOOST_TEST(TODOUint1CannotParse999);
* }
* // BOOST_TEST(!TypeTraits<UINT1>::canParse("999"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("1000"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("10000"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("50000"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("65534"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("65535"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("65536"));
*
* BOOST_TEST(!TypeTraits<UINT1>::canParse("1.0"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("x"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("1x"));
* BOOST_TEST(!TypeTraits<UINT1>::canParse("x1"));
* */
}


BOOST_AUTO_TEST_CASE(can_parse_uint2)
{
  using namespace dal;

  Uint2Type const type;

  BOOST_TEST(!type.canParse("-1"));
  BOOST_TEST(type.canParse("0"));
  BOOST_TEST(type.canParse("1"));
  BOOST_TEST(type.canParse("65535"));
  BOOST_TEST(!type.canParse("65536"));

  BOOST_TEST(!type.canParse("1.0"));
  BOOST_TEST(!type.canParse("x"));
  BOOST_TEST(!type.canParse("1x"));
  BOOST_TEST(!type.canParse("x1"));
}


BOOST_AUTO_TEST_CASE(can_parse_uint4)
{
  using namespace dal;

  Uint4Type const type;

  BOOST_TEST(!type.canParse("-1"));
  BOOST_TEST(type.canParse("0"));
  BOOST_TEST(type.canParse("1"));
  BOOST_TEST(type.canParse("65535"));
  BOOST_TEST(type.canParse("65536"));
  BOOST_TEST(type.canParse("4294967295"));
  BOOST_TEST(!type.canParse("4294967296"));

  BOOST_TEST(!type.canParse("1.0"));
  BOOST_TEST(!type.canParse("x"));
  BOOST_TEST(!type.canParse("1x"));
  BOOST_TEST(!type.canParse("x1"));
}


BOOST_AUTO_TEST_CASE(can_parse_int1)
{
  using namespace dal;

  Int1Type const type;

  BOOST_TEST(!type.canParse("-129"));
  BOOST_TEST(type.canParse("-128"));
  BOOST_TEST(type.canParse("-1"));
  BOOST_TEST(!type.canParse("-1.0"));
  BOOST_TEST(type.canParse("0"));
  BOOST_TEST(!type.canParse("0.0"));
  BOOST_TEST(!type.canParse(".0"));
  BOOST_TEST(!type.canParse("0."));
  BOOST_TEST(type.canParse("1"));
  BOOST_TEST(!type.canParse("1.0"));
  BOOST_TEST(!type.canParse(".1"));
  BOOST_TEST(type.canParse("127"));
  BOOST_TEST(!type.canParse("128"));
}


BOOST_AUTO_TEST_CASE(can_parse_int2)
{
  using namespace dal;

  Int2Type const type;

  BOOST_TEST(!type.canParse("-32769"));
  BOOST_TEST(type.canParse("-32768"));
  BOOST_TEST(type.canParse("-1"));
  BOOST_TEST(!type.canParse("-1.0"));
  BOOST_TEST(type.canParse("0"));
  BOOST_TEST(!type.canParse("0.0"));
  BOOST_TEST(!type.canParse(".0"));
  BOOST_TEST(!type.canParse("0."));
  BOOST_TEST(type.canParse("1"));
  BOOST_TEST(!type.canParse("1.0"));
  BOOST_TEST(!type.canParse(".1"));
  BOOST_TEST(type.canParse("32767"));
  BOOST_TEST(!type.canParse("32768"));
}


BOOST_AUTO_TEST_CASE(can_parse_int4)
{
  using namespace dal;

  Int4Type const type;

  BOOST_TEST(!type.canParse("-2147483649"));
  BOOST_TEST(type.canParse("-2147483648"));
  BOOST_TEST(type.canParse("-1"));
  BOOST_TEST(!type.canParse("-1.0"));
  BOOST_TEST(type.canParse("0"));
  BOOST_TEST(!type.canParse("0.0"));
  BOOST_TEST(!type.canParse(".0"));
  BOOST_TEST(!type.canParse("0."));
  BOOST_TEST(type.canParse("1"));
  BOOST_TEST(!type.canParse("1.0"));
  BOOST_TEST(!type.canParse(".1"));
  BOOST_TEST(type.canParse("2147483647"));
  BOOST_TEST(!type.canParse("2147483648"));
}


BOOST_AUTO_TEST_CASE(can_parse_real4)
{
  using namespace dal;

  Real4Type const type;

  BOOST_TEST(type.canParse("5.0"));
  BOOST_TEST(type.canParse("5"));
  BOOST_TEST(type.canParse(".5"));
  BOOST_TEST(type.canParse("5."));
  BOOST_TEST(type.canParse("-5"));

  BOOST_TEST(!type.canParse(""));
  BOOST_TEST(!type.canParse("a"));
  BOOST_TEST(!type.canParse("5.0a"));
  BOOST_TEST(!type.canParse("a5.0"));
  BOOST_TEST(!type.canParse("5.."));
  BOOST_TEST(!type.canParse("..5"));
  BOOST_TEST(!type.canParse("5..0"));
  BOOST_TEST(!type.canParse("5.0."));
  BOOST_TEST(!type.canParse("5.0.2"));

  BOOST_TEST(!type.canParse("5,0"));
  BOOST_TEST(!type.canParse(",0"));
  BOOST_TEST(!type.canParse("5,"));
}


BOOST_AUTO_TEST_CASE(can_parse_real8)
{
  using namespace dal;

  Real8Type const type;

  BOOST_TEST(type.canParse("5.0"));
  BOOST_TEST(type.canParse("5"));
  BOOST_TEST(type.canParse(".5"));
  BOOST_TEST(type.canParse("5."));

  BOOST_TEST(!type.canParse(""));
  BOOST_TEST(!type.canParse("a"));
  BOOST_TEST(!type.canParse("5.0a"));
  BOOST_TEST(!type.canParse("a5.0"));
  BOOST_TEST(!type.canParse("5.."));
  BOOST_TEST(!type.canParse("..5"));
  BOOST_TEST(!type.canParse("5..0"));
  BOOST_TEST(!type.canParse("5.0."));
  BOOST_TEST(!type.canParse("5.0.2"));

  BOOST_TEST(!type.canParse("5,0"));
  BOOST_TEST(!type.canParse(",0"));
  BOOST_TEST(!type.canParse("5,"));
}


BOOST_AUTO_TEST_CASE(type_traits)
{
  using namespace dal;

  BOOST_TEST(TypeTraits<UINT1>::typeId == TI_UINT1);
  BOOST_TEST(TypeTraits<UINT1>::csfCr == CR_UINT1);
  BOOST_TEST(TypeTraits<std::string>::typeId != TI_UINT1);
  BOOST_TEST(TypeTraits<std::string>::typeId == TI_STRING);

  TypeOfTypeId<TI_UINT1> const v;
  BOOST_TEST(sizeof(v) == size_t(1));
}


BOOST_AUTO_TEST_CASE(basic_type)
{
  using namespace dal;

  Type const& i2(Type::get(TI_INT2));
  BOOST_TEST(i2.size() == size_t(2));
  BOOST_TEST(i2.id() == TI_INT2);
  BOOST_TEST(i2.hasTrivialCopy());
}
