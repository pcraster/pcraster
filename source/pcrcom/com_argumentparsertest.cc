#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_ARGUMENTPARSERTEST
#include "com_argumentparsertest.h"
#define INCLUDED_COM_ARGUMENTPARSERTEST
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
#ifndef INCLUDED_COM_ARGUMENTPARSER
#include "com_argumentparser.h"
#define INCLUDED_COM_ARGUMENTPARSER
#endif



/*!
  \file
  This file contains the implementation of the ArgumentParserTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ARGUMENTPARSER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::ArgumentParserTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ArgumentParserTest> instance(new ArgumentParserTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ArgumentParserTest::testString, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ArgumentParserTest::testSize_t, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ArgumentParserTest::testInt, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ArgumentParserTest::testDouble, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ARGUMENTPARSER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::ArgumentParserTest::ArgumentParserTest()
{
}



//! setUp
void com::ArgumentParserTest::setUp()
{
}



//! tearDown
void com::ArgumentParserTest::tearDown()
{
}



void com::ArgumentParserTest::testString()
{
  ArgumentParser<std::string> parser;
  std::string result;

  result = parser.parse("bla");
  BOOST_CHECK(result == "bla");
  BOOST_CHECK(parser.length() == 3);
  BOOST_CHECK(parser.full());

  result = parser.parse("bla bla");
  BOOST_CHECK(result == "bla bla");
  BOOST_CHECK(parser.length() == 7);
  BOOST_CHECK(parser.full());

  result = parser.parse(" 012345 ");
  BOOST_CHECK(result == "012345");
  BOOST_CHECK(parser.length() == 8);
  BOOST_CHECK(parser.full());

  result = parser.parse("0.5");
  BOOST_CHECK(result == "0.5");
  BOOST_CHECK(parser.length() == 3);
  BOOST_CHECK(parser.full());
}



void com::ArgumentParserTest::testSize_t()
{
  ArgumentParser<size_t> parser;
  size_t result;

  result = parser.parse("bla");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("bla bla");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse(" 012345");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("012345");
  BOOST_CHECK(result == 12345);
  BOOST_CHECK(parser.length() == 6);
  BOOST_CHECK(parser.full());

  result = parser.parse("-012345");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("+012345");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("3.4");
  BOOST_CHECK(result == 3);
  BOOST_CHECK(parser.length() == 1);
  BOOST_CHECK(!parser.full());
}



void com::ArgumentParserTest::testInt()
{
  ArgumentParser<int> parser;
  int result;

  result = parser.parse("bla");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("bla bla");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse(" 012345");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("012345");
  BOOST_CHECK(result == 12345);
  BOOST_CHECK(parser.length() == 6);
  BOOST_CHECK(parser.full());

  result = parser.parse("-012345");
  BOOST_CHECK(result == -12345);
  BOOST_CHECK(parser.length() == 7);
  BOOST_CHECK(parser.full());

  result = parser.parse("+012345");
  BOOST_CHECK(result == 12345);
  BOOST_CHECK(parser.length() == 7);
  BOOST_CHECK(parser.full());

  result = parser.parse("3.4");
  BOOST_CHECK(result == 3);
  BOOST_CHECK(parser.length() == 1);
  BOOST_CHECK(!parser.full());
}



void com::ArgumentParserTest::testDouble()
{
  ArgumentParser<double> parser;
  double result;

  result = parser.parse("bla");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("bla bla");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse(" 012345");
  BOOST_CHECK(parser.length() == 0);
  BOOST_CHECK(!parser.full());

  result = parser.parse("012345");
  BOOST_CHECK(result == 12345);
  BOOST_CHECK(parser.length() == 6);
  BOOST_CHECK(parser.full());

  result = parser.parse("-012345");
  BOOST_CHECK(result == -12345);
  BOOST_CHECK(parser.length() == 7);
  BOOST_CHECK(parser.full());

  result = parser.parse("+012345");
  BOOST_CHECK(result == 12345);
  BOOST_CHECK(parser.length() == 7);
  BOOST_CHECK(parser.full());

  result = parser.parse("3.4");
  BOOST_CHECK(result == 3.4);
  BOOST_CHECK(parser.length() == 3);
  BOOST_CHECK(parser.full());
}

