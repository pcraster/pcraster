#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TSSOUTPUTVALUETEST
#include "calc_tssoutputvaluetest.h"
#define INCLUDED_CALC_TSSOUTPUTVALUETEST
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

#ifndef INCLUDED_IOMANIP
#include <iomanip>
#define INCLUDED_IOMANIP
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the TssOutputValueTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TSSOUTPUTVALUE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::TssOutputValueTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TssOutputValueTest> instance(new TssOutputValueTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TssOutputValueTest::testCppStream, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TSSOUTPUTVALUE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::TssOutputValueTest::TssOutputValueTest()
{
}



//! setUp
void calc::TssOutputValueTest::setUp()
{
}

//! tearDown
void calc::TssOutputValueTest::tearDown()
{
}

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// tests to assure migration from C to C++ stream went wel
void calc::TssOutputValueTest::testCppStream()
{
  {
  // VS_LDD, VS_BOOLEAN
  double vals[] = { 0, 1, 9 };
  for (size_t i =0; i < ARRAY_SIZE(vals); ++i) {
    std::ostringstream os;
    char buf[16];
    os << std::setw(4) << vals[i];
    sprintf(buf,"%4g",vals[i]);
    BOOST_CHECK(os.str() == std::string(buf));
  }
  }
  {
  // VS_ORDINAL VS_NOMINAL
  double vals[] = { 0, 1, 9,-12345,999999 };
  for (size_t i =0; i < ARRAY_SIZE(vals); ++i) {
    std::ostringstream os;
    char buf[16];
    os << std::setw(10) << vals[i];
    sprintf(buf,"%10g",vals[i]);
    BOOST_CHECK(os.str() == std::string(buf));
  }
  }
  {
  // VS_SCALAR VS_DIRECTIONAL
  double vals[] = { 0, 1, 9,-12345,999999,123.56,0.00004,-12345689101214.456748 };
  for (size_t i =0; i < ARRAY_SIZE(vals); ++i) {
    std::ostringstream os;
    char buf[16];
    os << std::setw(11) << vals[i];
    sprintf(buf,"%11.6g",vals[i]);
    BOOST_CHECK(os.str() == std::string(buf));
  }
  }
}
