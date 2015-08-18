#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_FUNCTIONSTEST
#include "com_functionstest.h"
#define INCLUDED_COM_FUNCTIONSTEST
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
#ifndef INCLUDED_COM_FUNCTIONS
#include "com_functions.h"
#define INCLUDED_COM_FUNCTIONS
#endif



/*!
  \file
  This file contains the implementation of the FunctionsTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FUNCTIONS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::FunctionsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FunctionsTest> instance(new FunctionsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FunctionsTest::testMinimum, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FunctionsTest::testMaximum, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FUNCTIONS MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::FunctionsTest::FunctionsTest()
{
}



//! setUp
void com::FunctionsTest::setUp()
{
}

//! tearDown
void com::FunctionsTest::tearDown()
{
}



void com::FunctionsTest::testMinimum()
{
  double min;
  double mv;
  pcr::setMV(mv);

  {
    double from[] = {  2.0,  5.0,  3.0 };
    min = minimum(from, from + 3);
    BOOST_CHECK(min ==  2.0);
  }

  {
    double from[] = { mv, mv, mv };
    min = minimum(from, from + 3);
    BOOST_CHECK(pcr::isMV(min));
  }
}



void com::FunctionsTest::testMaximum()
{
  double max;
  double mv;
  pcr::setMV(mv);

  {
    double from[] = {  2.0,  5.0,  3.0 };
    max = maximum(from, from + 3);
    BOOST_CHECK(max == 5.0);
  }

  {
    double from[] = { mv, mv, mv };
    max = maximum(from, from + 3);
    BOOST_CHECK(pcr::isMV(max));
  }
}
