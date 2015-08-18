#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINETEST
#include "com_commandlinetest.h"
#define INCLUDED_COM_COMMANDLINETEST
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
#ifndef INCLUDED_COM_COMMANDLINE
#include "com_commandline.h"
#define INCLUDED_COM_COMMANDLINE
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif



/*!
  \file
  This file contains the implementation of the CommandLineTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::CommandLineTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CommandLineTest> instance(new CommandLineTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineTest::testPositionalValue, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::CommandLineTest::CommandLineTest()

 :   d_commandLine(0)

{
}



//! setUp
void com::CommandLineTest::setUp()
{
  d_commandLine = new CommandLine("Application", "version", "app");
}



//! tearDown
void com::CommandLineTest::tearDown()
{
  delete d_commandLine;
}



void com::CommandLineTest::testConstructor()
{
  setUp();
  BOOST_CHECK(d_commandLine->name() == "Application");
  BOOST_CHECK(d_commandLine->version() == "version");
  BOOST_CHECK(d_commandLine->command() == "app");
  tearDown();
}



void com::CommandLineTest::testPositionalValue()
{
  setUp();
  {
    PositionalValue<int> intValue("An integer", "Integer");
    d_commandLine->clear();
    d_commandLine->addArgument(&intValue);
    char const* const argv[] = { "app", "5" };
    const size_t argc = ARRAY_SIZE(argv);
    d_commandLine->parse(argc - 1, argv + 1);
    BOOST_CHECK(intValue.isParsed());
  }

  {
    PositionalValue<int> intValue("An integer", "Integer");
    d_commandLine->clear();
    d_commandLine->addArgument(&intValue);
    char const* const argv[] = { "app", "five" };
    const size_t argc = ARRAY_SIZE(argv);
    try {
      d_commandLine->parse(argc - 1, argv + 1);
    }
    catch(CommandLineException const& exception) {
      BOOST_CHECK(exception.size() == 4);
      BOOST_CHECK(exception[0] == "Command line argument 'five': Error");
      BOOST_CHECK(exception[1] == "Can not parse value:");
      BOOST_CHECK(exception[2] == "five");
      BOOST_CHECK(exception[3] == "^");
    }
    BOOST_CHECK(!intValue.isParsed());
  }
  tearDown();
}



