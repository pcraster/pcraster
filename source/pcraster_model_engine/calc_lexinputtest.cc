#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LEXINPUTTEST
#include "calc_lexinputtest.h"
#define INCLUDED_CALC_LEXINPUTTEST
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
#ifndef INCLUDED_CALC_LEXINPUT
#include "calc_lexinput.h"
#define INCLUDED_CALC_LEXINPUT
#endif



/*!
  \file
  This file contains the implementation of the LexInputTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LEXINPUT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::LexInputTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<LexInputTest> instance(new LexInputTest());

  suite->add(BOOST_CLASS_TEST_CASE(&LexInputTest::testInstallStringScript, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF LEXINPUT MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::LexInputTest::LexInputTest()
{
}



//! setUp
void calc::LexInputTest::setUp()
{
}

//! tearDown
void calc::LexInputTest::tearDown()
{
}

void calc::LexInputTest::testInstallStringScript()
{
  std::string s("this is a test string");
  std::string r;
  LexInput li;
  const char *argv[1];
  argv[0]=s.c_str();
  li.installArgvScript(1,argv,false);
  int c;
  while( (c = li.getChar()) != EOF)
    r+=(char)c;
  // need for an additonal newline
  BOOST_CHECK(r==(s+" "));

  // test piece of code from calc_lexinputsource.cc:
  // erase sealed contents from memory
  std::string contents("pietpaal");
  contents.replace(contents.begin(), contents.end(), contents.size(), 0);
  BOOST_CHECK(contents.size()==8);
  for(size_t i=0; i<contents.size(); i++)
    BOOST_CHECK(contents[i]==0);
}

