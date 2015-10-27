#define BOOST_TEST_MODULE pcraster com parsers
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_parsers.h"


BOOST_AUTO_TEST_CASE(comment_parser)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(section_header_parser)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(number_parser)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(variable_name_parser)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(filename_parser)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(function_call_parser)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(expression_parser)
{
}
