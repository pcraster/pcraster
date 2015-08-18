#ifndef INCLUDED_DAL_TYPETEST
#include "dal_TypeTest.h"
#define INCLUDED_DAL_TYPETEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif
#ifndef INCLUDED_DAL_BASICTYPES
#include "dal_BasicTypes.h"
#define INCLUDED_DAL_BASICTYPES
#endif


/*!
  \file
  This file contains the implementation of the TypeTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TYPE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::TypeTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TypeTest> instance(new TypeTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testId, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseUint1, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseUint2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseUint4, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseInt1, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseInt2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseInt4, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseReal4, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testCanParseReal8, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testTypeTraits, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypeTest::testBasicType, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TYPE MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::TypeTest::TypeTest()
{
}



//! setUp
void dal::TypeTest::setUp()
{
}



//! tearDown
void dal::TypeTest::tearDown()
{
}



void dal::TypeTest::testId()
{
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



void dal::TypeTest::testCanParseUint1()
{
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



void dal::TypeTest::testCanParseUint2()
{
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



void dal::TypeTest::testCanParseUint4()
{
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



void dal::TypeTest::testCanParseInt1()
{
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



void dal::TypeTest::testCanParseInt2()
{
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



void dal::TypeTest::testCanParseInt4()
{
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



void dal::TypeTest::testCanParseReal4()
{
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



void dal::TypeTest::testCanParseReal8()
{
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

void dal::TypeTest::testTypeTraits()
{
  BOOST_CHECK_EQUAL(TypeTraits<UINT1>::typeId, TI_UINT1);
  BOOST_CHECK_EQUAL(TypeTraits<UINT1>::csfCr, CR_UINT1);
  BOOST_CHECK(TypeTraits<std::string>::typeId != TI_UINT1);
  BOOST_CHECK_EQUAL(TypeTraits<std::string>::typeId, TI_STRING);

  TypeOfTypeId<TI_UINT1> v;
  BOOST_CHECK_EQUAL(sizeof(v), size_t(1));
}

void dal::TypeTest::testBasicType()
{
  Type const& i2(Type::get(TI_INT2));
  BOOST_CHECK_EQUAL(i2.size(), size_t(2));
  BOOST_CHECK_EQUAL(i2.id(), TI_INT2);
  BOOST_CHECK(i2.hasTrivialCopy());
}

