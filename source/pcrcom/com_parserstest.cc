#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PARSERSTEST
#include "com_parserstest.h"
#define INCLUDED_COM_PARSERSTEST
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
#ifndef INCLUDED_COM_PARSERS
#include "com_parsers.h"
#define INCLUDED_COM_PARSERS
#endif



/*!
  \file
  This file contains the implementation of the ParsersTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARSERS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::ParsersTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ParsersTest> instance(new ParsersTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testCommentParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testSectionHeaderParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testNumberParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testVariableNameParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testFileNameParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testFunctionCallParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParsersTest::testExpressionParser, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PARSERS MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::ParsersTest::ParsersTest()
{
}



//! setUp
void com::ParsersTest::setUp()
{
}

//! tearDown
void com::ParsersTest::tearDown()
{
}



void com::ParsersTest::testCommentParser()
{
  CommentGrammar parser;

  std::vector<std::string> valid, invalid;
  valid.push_back("#");
  valid.push_back("##");
  valid.push_back("# #");
  valid.push_back("# # ");
  valid.push_back("#blabla");
  valid.push_back("# blabla");
  valid.push_back("# bla bla bla bla #");

  invalid.push_back("");
  invalid.push_back("   ");
  invalid.push_back(" #");
  invalid.push_back(" #####");
  invalid.push_back(" # # # # #");
  invalid.push_back("//");
  invalid.push_back("//#");
  invalid.push_back("// # bla");
  invalid.push_back("/* # bla");
  invalid.push_back("/* # bla */");

  for(std::vector<std::string>::const_iterator it = valid.begin();
         it != valid.end(); ++it) {
    BOOST_CHECK(boost::spirit::parse((*it).c_str(), parser).full);
  }

  for(std::vector<std::string>::const_iterator it = invalid.begin();
         it != invalid.end(); ++it) {
    BOOST_CHECK(!boost::spirit::parse((*it).c_str(), parser).full);
  }
}



void com::ParsersTest::testSectionHeaderParser()
{
  SectionHeaderGrammar parser;

  std::vector<std::string> valid, invalid;
  valid.push_back("binding");
  valid.push_back("areamap");
  valid.push_back("timer");
  valid.push_back("initial");
  valid.push_back("dynamic");

  invalid.push_back("");
  invalid.push_back(" binding");
  invalid.push_back("binding ");
  invalid.push_back(" binding ");

  for(std::vector<std::string>::const_iterator it = valid.begin();
         it != valid.end(); ++it) {
    BOOST_CHECK(boost::spirit::parse((*it).c_str(), parser).full);
  }

  for(std::vector<std::string>::const_iterator it = invalid.begin();
         it != invalid.end(); ++it) {
    BOOST_CHECK(!boost::spirit::parse((*it).c_str(), parser).full);
  }
}



void com::ParsersTest::testNumberParser()
{
  NumberGrammar parser;

  std::vector<std::string> valid, invalid;
  valid.push_back("0");
  valid.push_back("1");
  valid.push_back("0.1");
  valid.push_back("1e32");

  invalid.push_back("");
  invalid.push_back("a");
  invalid.push_back(" ");
  invalid.push_back(" 5");

  for(std::vector<std::string>::const_iterator it = valid.begin();
         it != valid.end(); ++it) {
    BOOST_CHECK(boost::spirit::parse((*it).c_str(), parser).full);
  }

  for(std::vector<std::string>::const_iterator it = invalid.begin();
         it != invalid.end(); ++it) {
    BOOST_CHECK(!boost::spirit::parse((*it).c_str(), parser).full);
  }
}



void com::ParsersTest::testVariableNameParser()
{
  VariableNameGrammar parser;

  std::vector<std::string> valid, invalid;
  valid.push_back("a");
  valid.push_back("abracadr");
  valid.push_back("fileName");
  valid.push_back("fileName5");
  // valid.push_back("file_name");
  valid.push_back("BlablaBla");
  valid.push_back("PRRDDLKJ");
  valid.push_back("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");

  invalid.push_back("");
  invalid.push_back(" ");
  invalid.push_back("5fileName");
  invalid.push_back("55");
  // invalid.push_back("5(");
  // invalid.push_back("(");
  // invalid.push_back("f()");

  for(std::vector<std::string>::const_iterator it = valid.begin();
         it != valid.end(); ++it) {
    BOOST_CHECK(boost::spirit::parse((*it).c_str(), parser).full);
  }

  for(std::vector<std::string>::const_iterator it = invalid.begin();
         it != invalid.end(); ++it) {
    BOOST_CHECK(!boost::spirit::parse((*it).c_str(), parser).full);
  }
}



void com::ParsersTest::testFileNameParser()
{
  FileNameGrammar parser;

  std::vector<std::string> valid, invalid;
  valid.push_back("a");
  valid.push_back("a.b");
  valid.push_back("a.bcd");
  valid.push_back("abcdefgh");
  valid.push_back("abcdefgh.");
  valid.push_back("abcdefgh.i");
  valid.push_back("abcdefgh.ijk");
  valid.push_back("AbCdEfGh.iJk");

  invalid.push_back("");
  invalid.push_back(" ");
  invalid.push_back("abcdefghi");

  for(std::vector<std::string>::const_iterator it = valid.begin();
         it != valid.end(); ++it) {
    BOOST_CHECK(boost::spirit::parse((*it).c_str(), parser).full);
  }

  for(std::vector<std::string>::const_iterator it = invalid.begin();
         it != invalid.end(); ++it) {
    BOOST_CHECK(!boost::spirit::parse((*it).c_str(), parser).full);
  }
}



void com::ParsersTest::testFunctionCallParser()
{
/*
  // boost::spirit::rule<> parser = functionCallParser();
  boost::spirit::rule<> funNameParser = variableNameGrammar();
  boost::spirit::rule<> numParser = numberParser();
  boost::spirit::rule<> varNameParser = variableNameGrammar();
  boost::spirit::rule<> exprParser = numParser | varNameParser;

  // ts_num_p = (uint3_p >> *(',' >> uint3_3_p));

  boost::spirit::rule<> parser = funNameParser >> '(' >> !(exprParser >> *(',' >> exprParser)) >> ')';

  // expression = number | variable | fileName | functionCall

  std::vector<std::string> valid, invalid;
  valid.push_back("f()");
  valid.push_back("f(a)");
  valid.push_back("f(5,5)");
  valid.push_back("f(1,a,2,b,3,c)");
  valid.push_back("f(g())");
  valid.push_back("f(g(), h())");
  valid.push_back("f(g(), 5)");
  valid.push_back("f(g(h(5)))");

  invalid.push_back("");
  invalid.push_back(" ");
  invalid.push_back("f");
  invalid.push_back("()");
  invalid.push_back("f[]");
  invalid.push_back("f{}");

  for(std::vector<std::string>::const_iterator it = valid.begin();
         it != valid.end(); ++it) {
    BOOST_CHECK(boost::spirit::parse((*it).c_str(), parser).full);
  }

  for(std::vector<std::string>::const_iterator it = invalid.begin();
         it != invalid.end(); ++it) {
    BOOST_CHECK(!boost::spirit::parse((*it).c_str(), parser).full);
  }
  */
}



void com::ParsersTest::testExpressionParser()
{
}



