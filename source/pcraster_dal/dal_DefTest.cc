#ifndef INCLUDED_DAL_DEFTEST
#include "dal_DefTest.h"
#define INCLUDED_DAL_DEFTEST
#endif

// Library headers.
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

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
#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif


/*!
  \file
  This file contains the implementation of the DefTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DEF MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::DefTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DefTest> instance(new DefTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DefTest::testTypeSizes, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DEF MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::DefTest::DefTest()
{
}



//! setUp
void dal::DefTest::setUp()
{
}



//! tearDown
void dal::DefTest::tearDown()
{
}



void dal::DefTest::testTypeSizes()
{
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
