#ifndef INCLUDED_DAL_TYPESTEST
#include "dal_TypesTest.h"
#define INCLUDED_DAL_TYPESTEST
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
#ifndef INCLUDED_DAL_TYPES
#include "dal_Types.h"
#define INCLUDED_DAL_TYPES
#endif




/*!
  \file
  This file contains the implementation of the TypesTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TYPES MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::TypesTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TypesTest> instance(new TypesTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TypesTest::testIdOfSmallestType, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TypesTest::testIdOfLargestType, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TYPES MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::TypesTest::TypesTest()
{
}



//! setUp
void dal::TypesTest::setUp()
{
}



//! tearDown
void dal::TypesTest::tearDown()
{
}



void dal::TypesTest::testIdOfSmallestType()
{
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



void dal::TypesTest::testIdOfLargestType()
{
  Types types;
  BOOST_CHECK_EQUAL(types.idOfLargestType(TI_UINT1, TI_UINT2), TI_UINT2);

  bool idOfLargestTypeImplemented = false;
  BOOST_WARN(idOfLargestTypeImplemented);
}
