#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_BINARYOPERATORSTEST
#include "com_binaryoperatorstest.h"
#define INCLUDED_COM_BINARYOPERATORSTEST
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
#ifndef INCLUDED_COM_BINARYOPERATORS
#include "com_binaryoperators.h"
#define INCLUDED_COM_BINARYOPERATORS
#endif



/*!
  \file
  This file contains the implementation of the BinaryOperatorsTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINARYOPERATORS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::BinaryOperatorsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BinaryOperatorsTest> instance(new BinaryOperatorsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BinaryOperatorsTest::testDivide, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BinaryOperatorsTest::testMVCast, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BINARYOPERATORS MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::BinaryOperatorsTest::BinaryOperatorsTest()
{
}



//! setUp
void com::BinaryOperatorsTest::setUp()
{
}

//! tearDown
void com::BinaryOperatorsTest::tearDown()
{
}

void com::BinaryOperatorsTest::testMVCast()
{
  UINT1 to[] = {  2,  5,  3};
  UINT1 from = MV_UINT1;

 addValue(to, to+3, from);

 BOOST_CHECK(to[0]==MV_UINT1);
}


void com::BinaryOperatorsTest::testDivide()
{
  double to[]   = { 10.0, 20.0, 30.0 };
  double from[] = {  2.0,  5.0,  3.0 };

  divideByRange(to, to + 3, from);

  BOOST_CHECK(to[0] ==  5.0);
  BOOST_CHECK(to[1] ==  4.0);
  BOOST_CHECK(to[2] == 10.0);

  divideByValue(to, to + 3, 2.0);
  BOOST_CHECK(to[0] == 2.5);
  BOOST_CHECK(to[1] == 2.0);
  BOOST_CHECK(to[2] == 5.0);

}
