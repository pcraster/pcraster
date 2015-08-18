#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DIMENSIONPARSERTEST
#include "calc_dimensionparsertest.h"
#define INCLUDED_CALC_DIMENSIONPARSERTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_DIMENSIONPARSER
#include "calc_dimensionparser.h"
#define INCLUDED_CALC_DIMENSIONPARSER
#endif



/*!
  \file
  This file contains the implementation of the DimensionParserTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIMENSIONPARSER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::DimensionParserTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DimensionParserTest> instance(new DimensionParserTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DimensionParserTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DimensionParserTest::testError, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DIMENSIONPARSER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::DimensionParserTest::DimensionParserTest()
{
}



//! setUp
void calc::DimensionParserTest::setUp()
{
}



//! tearDown
void calc::DimensionParserTest::tearDown()
{
}



void calc::DimensionParserTest::test()
{
 {
  DimensionParser dp("a 4");
  BOOST_CHECK(dp.d_symbols.size()==1);
  BOOST_CHECK(dp.d_symbols[0].d_symbol == "a");
  BOOST_CHECK(dp.d_symbols[0].d_power  == 4);
 }
 {
  DimensionParser dp("m2s-1");
  BOOST_CHECK(dp.d_symbols.size()==2);
  BOOST_CHECK(dp.d_symbols[0].d_symbol == "m");
  BOOST_CHECK(dp.d_symbols[0].d_power  == 2);
  BOOST_CHECK(dp.d_symbols[1].d_symbol == "s");
  BOOST_CHECK(dp.d_symbols[1].d_power  == -1);
 }
 {
  DimensionParser dp("kg,m");
  BOOST_CHECK(dp.d_symbols.size()==2);
  BOOST_CHECK(dp.d_symbols[0].d_symbol == "kg");
  BOOST_CHECK(dp.d_symbols[0].d_power  == 1);
  BOOST_CHECK(dp.d_symbols[1].d_symbol == "m");
  BOOST_CHECK(dp.d_symbols[1].d_power  == 1);
 }
}

void calc::DimensionParserTest::testError()
{

 try {
  DimensionParser dp("kg;m");
 } catch (const com::Exception& e) {
BOOST_CHECK(e.messages().find("';' is not a recognized unit dimension")
   != std::string::npos);
 }
 try {
  DimensionParser dp("X kg");
 } catch (const com::Exception& e) {
BOOST_CHECK(e.messages().find("X") != std::string::npos);
 }

}
