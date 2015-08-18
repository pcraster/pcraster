#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENTSTEST
#include "com_commandlineargumentstest.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTSTEST
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
#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENTS
#include "com_commandlinearguments.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTS
#endif


/*!
  \file
  This file contains the implementation of the CommandLineArgumentsTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINEARGUMENTS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::CommandLineArgumentsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CommandLineArgumentsTest> instance(new CommandLineArgumentsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentsTest::testOptions, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINEARGUMENTS MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::CommandLineArgumentsTest::CommandLineArgumentsTest()
{
}



//! setUp
void com::CommandLineArgumentsTest::setUp()
{
}

//! tearDown
void com::CommandLineArgumentsTest::tearDown()
{
}



void com::CommandLineArgumentsTest::testOptions()
{
  {
    char c1[4] = { "cmd" };
    char c2[3] = { "-a" };
    char c3[3] = { "-b" };
    char* argv[] = {c1, c2, c3 };
    const size_t argc = ARRAY_SIZE(argv);

    com::Option arg1('a', "aa", "An argument", true);
    com::Option arg2('b', "bb", "An argument", true);

    com::CommandLineArguments arguments;
    arguments.add(&arg1);
    arguments.add(&arg2);

    BOOST_CHECK(!arg1.isParsed());
    BOOST_CHECK(!arg2.isParsed());
    BOOST_CHECK(arg1.isRequired());
    BOOST_CHECK(arg2.isRequired());

    arguments.parse(argc - 1, argv + 1);

    BOOST_CHECK(arg1.isParsed());
    BOOST_CHECK(arg2.isParsed());
    BOOST_CHECK(arg1.isRequired());
    BOOST_CHECK(arg2.isRequired());
  }

  // Terse mode.
  {
    char c1[4] = { "cmd" };
    char c2[4] = { "-ab" };
    char* argv[] = { c1, c2 };
    const size_t argc = ARRAY_SIZE(argv);
    POSTCOND(argc == 2);

    com::Option arg1('a', "aa", "An argument", true);
    com::Option arg2('b', "bb", "An argument", true);

    com::CommandLineArguments arguments;
    arguments.add(&arg1);
    arguments.add(&arg2);

    BOOST_CHECK(!arg1.isParsed());
    BOOST_CHECK(!arg2.isParsed());
    BOOST_CHECK(arg1.isRequired());
    BOOST_CHECK(arg2.isRequired());

    arguments.parse(argc - 1, argv + 1);

    BOOST_CHECK(arg1.isParsed());
    BOOST_CHECK(arg2.isParsed());
    BOOST_CHECK(arg1.isRequired());
    BOOST_CHECK(arg2.isRequired());
  }
  // OptionValues
  {
    char c1[4] = { "cmd" };
    char c2[4] = { "-ab" };
    char c3[4] = { "bla" };
    char *argv[] = { c1, c2, c3 };
    const size_t argc = ARRAY_SIZE(argv);
    POSTCOND(argc == 3);

    com::Option                   arg1('a', "aa", "An argument", true);
    com::OptionValue<std::string> arg2('b', "bb", "An argument", "Nog iets", true);

    com::CommandLineArguments arguments;
    arguments.add(&arg1);
    arguments.add(&arg2);

    BOOST_CHECK(!arg1.isParsed());
    BOOST_CHECK(!arg2.isParsed());
    BOOST_CHECK(arg1.isRequired());
    BOOST_CHECK(arg2.isRequired());

    arguments.parse(argc - 1, argv + 1);

    BOOST_CHECK(arg1.isParsed());
    BOOST_CHECK(arg2.isParsed());
    BOOST_CHECK(arg2.value() == "bla");
    BOOST_CHECK(arg1.isRequired());
    BOOST_CHECK(arg2.isRequired());
  }
}
