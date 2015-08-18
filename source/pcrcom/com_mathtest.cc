#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_MATHTEST
#include "com_mathtest.h"
#define INCLUDED_COM_MATHTEST
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
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif


/*!
  \file
  This file contains the implementation of the MathTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATH MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::MathTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MathTest> instance(new MathTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MathTest::testEqualEpsilon, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MathTest::testLim, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MathTest::testIsInteger, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MathTest::testInterpolate2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MathTest::testMinimizeMaximize, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MATH MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::MathTest::MathTest()
{
}



//! setUp
void com::MathTest::setUp()
{
}

//! tearDown
void com::MathTest::tearDown()
{
}



void com::MathTest::testEqualEpsilon()
{
  // one of them really 0 makes fraction BIG
  BOOST_CHECK( equal_epsilon<double>(0, 0.00001, 0.00001));

  BOOST_CHECK(equal_epsilon<double>(1,1));
  BOOST_CHECK(!equal_epsilon<double>(1,2));

  BOOST_CHECK(equal_epsilon<double>(1, 1.00001, 0.1));
  BOOST_CHECK(equal_epsilon<double>(1, 1.00001, 0.00001));

  BOOST_CHECK( equal_epsilon<double>(5, 5.00001, 0.00001));
  BOOST_CHECK( equal_epsilon<double>(5, 5.00001, 0.000005));
  BOOST_CHECK(!equal_epsilon<double>(5, 5.00001, 0.0000005));

  BOOST_CHECK(equal_epsilon<double>(std::sqrt(2.0), 1.41, 0.1));
  BOOST_CHECK(equal_epsilon<double>(std::sqrt(2.0), 1.41, 0.01));

  BOOST_CHECK(fractionDifference<double>(2, 2)==0);

  BOOST_CHECK(fractionDifference<double>(2, 1)==0.5);
  BOOST_CHECK(fractionDifference<double>(1, 2)==0.5);
}

void com::MathTest::testIsInteger()
{
  BOOST_CHECK(isInteger(0.0));
  BOOST_CHECK(isInteger(-100.0));
  BOOST_CHECK(isInteger(  99.0));

  BOOST_CHECK(!isInteger(0.000001));
  BOOST_CHECK(!isInteger(-99.000007));
  BOOST_CHECK(!isInteger(111.07));
}

void com::MathTest::testInterpolate2()
{
  BOOST_CHECK(interpolate2(1.5, 1, 3, 2, 4) == 3.5);
  BOOST_CHECK(interpolate2(1.5, 2, 4, 1, 3) == 3.5);
  BOOST_CHECK(interpolate2(0, -1, -1, 1, 1) == 0);
  BOOST_CHECK(interpolate2(0, -3, -3, 1, 1) == 0);
  BOOST_CHECK(equal_epsilon<double>(interpolate2(0, -4, 0, 1, 1),0.8));

  BOOST_CHECK(interpolate2(0, -3, -3, -3, -3) == -3);
  BOOST_CHECK(interpolate2(0, 0, 0, 1, 1) == 0);
}

void com::MathTest::testMinimizeMaximize()
{
  {
    int x=-1,y=3;
    maximize(x,y);
    BOOST_CHECK(x==3);
    maximize(x,8);
    BOOST_CHECK(x==8);
    minimize(x,y);
    BOOST_CHECK(x==3);
    minimize(x,-9);
    BOOST_CHECK(x==-9);
  }
  {
    double x=-1,y=3;
    maximize(x,y);
    BOOST_CHECK(x==3);
    maximize<double>(x,8);
    BOOST_CHECK(x==8);
    minimize(x,y);
    BOOST_CHECK(x==3);
    minimize(x,-9.0);
    BOOST_CHECK(x==-9);
  }
}

void com::MathTest::testLim()
{
  BOOST_CHECK(lim<int>(2,4,6) == 4);
  BOOST_CHECK(lim<int>(8,4,6) == 6);
  BOOST_CHECK(lim<int>(5,4,6) == 5);

  BOOST_CHECK(limUnordered<int>(2,4,6) == 4);
  BOOST_CHECK(limUnordered<int>(8,4,6) == 6);
  BOOST_CHECK(limUnordered<int>(5,4,6) == 5);

  BOOST_CHECK(limUnordered<int>(2,6,4) == 4);
  BOOST_CHECK(limUnordered<int>(8,6,4) == 6);
  BOOST_CHECK(limUnordered<int>(5,6,4) == 5);
}
