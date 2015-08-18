#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENTTEST
#include "com_commandlineargumenttest.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTTEST
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

#ifndef INCLUDED_COM_EXCLUSIVEARGUMENT
#include "com_exclusiveargument.h"
#define INCLUDED_COM_EXCLUSIVEARGUMENT
#endif

#ifndef INCLUDED_COM_REPEATABLEARGUMENT
#include "com_repeatableargument.h"
#define INCLUDED_COM_REPEATABLEARGUMENT
#endif

#ifndef INCLUDED_COM_REPEATABLEEXCLUSIVEARGUMENT
#include "com_repeatableexclusiveargument.h"
#define INCLUDED_COM_REPEATABLEEXCLUSIVEARGUMENT
#endif



/*!
  \file
  This file contains the implementation of the CommandLineArgumentTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINEARGUMENT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::CommandLineArgumentTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CommandLineArgumentTest> instance(new CommandLineArgumentTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testPositionalValue, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testPositionalList, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testOption, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testOptionValue, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testOptionList, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testRepeatableArgument, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testExclusiveArgument, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testRepeatableExclusiveArgument, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CommandLineArgumentTest::testArgumentSort, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINEARGUMENT MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::CommandLineArgumentTest::CommandLineArgumentTest()
{
}



//! setUp
void com::CommandLineArgumentTest::setUp()
{
}

//! tearDown
void com::CommandLineArgumentTest::tearDown()
{
}



void com::CommandLineArgumentTest::testPositionalValue()
{
  char c1[4] = { "cmd" };
  char c2[5] = { "arg1" };
  char c3[3] = { "-f" };
  char c4[10] = { "file-name" };
  char* const argv[] = { c1, c2, c3, c4 };
  const size_t argc = ARRAY_SIZE(argv);

  PositionalValue<std::string> arg1("test argument 1", "Just a test argument",
                   true);
  PositionalValue<std::string> arg2("test argument 2", "Just a test argument",
                   true);

  BOOST_CHECK_EQUAL(arg1.isRequired(), true);
  BOOST_CHECK_EQUAL(arg2.isRequired(), true);
  BOOST_CHECK_EQUAL(arg1.isParsed(), false);
  BOOST_CHECK_EQUAL(arg2.isParsed(), false);

  size_t nrTokensParsed;

  BOOST_CHECK(arg1.canParse(*(argv + 1)));
  nrTokensParsed = arg1.parse(argc - 1, argv + 1);
  BOOST_CHECK_EQUAL(nrTokensParsed, (size_t)1);
  BOOST_CHECK_EQUAL(arg1.isParsed(), true);
  BOOST_CHECK_EQUAL(arg1.value(), "arg1");

  BOOST_CHECK(!arg2.canParse(*(argv + 1 + nrTokensParsed)));
  BOOST_CHECK_EQUAL(arg2.isParsed(), false);
  BOOST_CHECK_EQUAL(arg2.value(), "");

  {
    char c1[2] = { "5" };
    char* const argv[] = { c1 };
    const size_t argc = ARRAY_SIZE(argv);

    PositionalValue<int> positional("radius", "Neighbourhood radius", true);

    BOOST_CHECK_EQUAL(positional.isRequired(), true);
    BOOST_CHECK_EQUAL(positional.isParsed(), false);
    BOOST_CHECK(positional.canParse(*argv));
    nrTokensParsed = positional.parse(argc, argv);
    BOOST_CHECK_EQUAL(nrTokensParsed, (size_t)1);
    BOOST_CHECK_EQUAL(positional.isParsed(), true);
    BOOST_CHECK_EQUAL(positional.value(), 5);
  }

  {
    char c1[4] = { "5.5" };
    char* const argv[] = { c1 };
    const size_t argc = ARRAY_SIZE(argv);

    PositionalValue<int> positional("radius", "Neighbourhood radius", true);

    BOOST_CHECK_EQUAL(positional.isRequired(), true);
    BOOST_CHECK_EQUAL(positional.isParsed(), false);
    BOOST_CHECK(positional.canParse(*argv));
    try {
      nrTokensParsed = positional.parse(argc, argv);
    }
    catch(com::CommandLineException& exception) {
      BOOST_CHECK_EQUAL(exception.size(), (size_t)3);
      BOOST_CHECK_EQUAL(exception[0], "Can not parse value:");
      BOOST_CHECK_EQUAL(exception[1], "5.5");
      BOOST_CHECK_EQUAL(exception[2], "-^");
    }
    BOOST_CHECK_EQUAL(positional.isParsed(), false);
  }
}



void com::CommandLineArgumentTest::testPositionalList()
{
  char c1[4] = { "cmd" };
  char c2[5] = { "arg1" };
  char c3[10] = { "file-name" };
  char c4[3] = { "-f" };
  char* const argv[] = { c1, c2, c3, c4 };
  const size_t argc = ARRAY_SIZE(argv);

  PositionalList<std::string> arg1("test argument 1", "Just a test argument",
                   true);
  PositionalValue<std::string> arg2("test argument 2", "Just a test argument",
                   true);

  BOOST_CHECK_EQUAL(arg1.isRequired(), true);
  BOOST_CHECK_EQUAL(arg2.isRequired(), true);
  BOOST_CHECK_EQUAL(arg1.isParsed(), false);
  BOOST_CHECK_EQUAL(arg2.isParsed(), false);

  size_t nrTokensParsed;

  BOOST_CHECK(arg1.canParse(*(argv + 1)));
  nrTokensParsed = arg1.parse(argc - 1, argv + 1);
  BOOST_CHECK_EQUAL(nrTokensParsed, (size_t)2);
  BOOST_CHECK_EQUAL(arg1.isParsed(), true);
  BOOST_CHECK_EQUAL(arg1.value(0), "arg1");
  BOOST_CHECK_EQUAL(arg1.value(1), "file-name");

  BOOST_CHECK(!arg2.canParse(*(argv + 1 + nrTokensParsed)));
  BOOST_CHECK_EQUAL(arg2.isParsed(), false);

  {
    char c1[4] = { "5.5" };
    char* const argv[] = { c1 };
    const size_t argc = ARRAY_SIZE(argv);

    PositionalList<int> positional("value", "Values", true);

    BOOST_CHECK_EQUAL(positional.isRequired(), true);
    BOOST_CHECK_EQUAL(positional.isParsed(), false);
    BOOST_CHECK(positional.canParse(*argv));
    try {
      nrTokensParsed = positional.parse(argc, argv);
    }
    catch(com::CommandLineException& exception) {
      BOOST_CHECK_EQUAL(exception.size(), (size_t)3);
      BOOST_CHECK_EQUAL(exception[0], "Can not parse value:");
      BOOST_CHECK_EQUAL(exception[1], "5.5");
      BOOST_CHECK_EQUAL(exception[2], "-^");
    }
    BOOST_CHECK_EQUAL(positional.isParsed(), false);
  }
}



void com::CommandLineArgumentTest::testOption()
{
  {
    Option optionA('a', "aaaa", "just an option", true);
    Option optionB('b', "bbbb", "just an option", true);
    Option optionC('c', "cccc", "just an option", true);

    BOOST_CHECK_EQUAL(optionA.isRequired(), true);
    BOOST_CHECK_EQUAL(optionB.isRequired(), true);
    BOOST_CHECK_EQUAL(optionC.isRequired(), true);
    BOOST_CHECK_EQUAL(optionA.isParsed(), false);
    BOOST_CHECK_EQUAL(optionB.isParsed(), false);
    BOOST_CHECK_EQUAL(optionC.isParsed(), false);
  }

  {
    char c1[4] = { "cmd" };
    char c2[3] = { "-a" };
    char c3[3] = { "-b" };
    char c4[3] = { "-c" };
    char* const argv[] = { c1, c2, c3, c4 };
    const size_t argc = ARRAY_SIZE(argv);

    Option optionA('a', "aaaa", "just an option", true);
    Option optionB('b', "bbbb", "just an option", true);
    Option optionC('c', "cccc", "just an option", true);

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, (size_t)1);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);

    BOOST_CHECK(optionB.canParse(*(argv + 2)));
    nrTokensParsed = optionB.parse(argc - 2, argv + 2);
    BOOST_CHECK_EQUAL(nrTokensParsed, (size_t)1);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);

    BOOST_CHECK(optionC.canParse(*(argv + 3)));
    nrTokensParsed = optionC.parse(argc - 3, argv + 3);
    BOOST_CHECK_EQUAL(nrTokensParsed, (size_t)1);
    BOOST_CHECK_EQUAL(optionC.isParsed(), true);
  }

  {
    char * argv[2];
    argv[0] = new char[4];
    argv[1] = new char[5];

    strcpy(argv[0], "cmd");
    strcpy(argv[1], "-abc");
    const size_t argc = ARRAY_SIZE(argv);

    Option optionA('a', "aaaa", "just an option", true);
    Option optionB('b', "bbbb", "just an option", true);
    Option optionC('c', "cccc", "just an option", true);

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 0);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);

    BOOST_CHECK(optionB.canParse(*(argv + 1)));
    nrTokensParsed = optionB.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 0);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);

    BOOST_CHECK(optionC.canParse(*(argv + 1)));
    nrTokensParsed = optionC.parse(argc - 1, argv + 1);
    // Now the whole token is parsed.
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionC.isParsed(), true);

    delete [] argv[0];
    delete [] argv[1];
  }
}



void com::CommandLineArgumentTest::testOptionValue()
{
  {
    OptionValue<std::string> optionA('a', "aaaa", "an argument",
           "Just an argument", true);
    OptionValue<std::string> optionB('b', "bbbb", "an argument",
           "Just an argument", true);
    OptionValue<std::string> optionC('c', "cccc", "an argument",
           "Just an argument", true);

    BOOST_CHECK_EQUAL(optionA.isRequired(), true);
    BOOST_CHECK_EQUAL(optionB.isRequired(), true);
    BOOST_CHECK_EQUAL(optionC.isRequired(), true);
    BOOST_CHECK_EQUAL(optionA.isParsed(), false);
    BOOST_CHECK_EQUAL(optionB.isParsed(), false);
    BOOST_CHECK_EQUAL(optionC.isParsed(), false);
  }

  {
    char c1[4] = { "cmd" };
    char c2[3] = { "-a" };
    char c3[7] = { "valueA" };
    char c4[3] = { "-b" };
    char c5[7] = { "valueB" };
    char c6[3] = { "-c" };
    char c7[7] = { "valueC" };
    char* const argv[] = { c1, c2, c3, c4, c5, c6, c7 };
    const size_t argc = ARRAY_SIZE(argv);

    OptionValue<std::string> optionA('a', "aaaa", "an argument",
           "Just an argument", true);
    OptionValue<std::string> optionB('b', "bbbb", "an argument",
           "Just an argument", true);
    OptionValue<std::string> optionC('c', "cccc", "an argument",
           "Just an argument", true);

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 2);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);
    BOOST_CHECK_EQUAL(optionA.value(), "valueA");

    BOOST_CHECK(optionB.canParse(*(argv + 3)));
    nrTokensParsed = optionB.parse(argc - 3, argv + 3);
    BOOST_CHECK_EQUAL(nrTokensParsed, 2);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);
    BOOST_CHECK_EQUAL(optionB.value(), "valueB");

    BOOST_CHECK(optionC.canParse(*(argv + 5)));
    nrTokensParsed = optionC.parse(argc - 5, argv + 5);
    BOOST_CHECK_EQUAL(nrTokensParsed, 2);
    BOOST_CHECK_EQUAL(optionC.isParsed(), true);
    BOOST_CHECK_EQUAL(optionC.value(), "valueC");
  }

  {
    char c1[4] = { "cmd" };
    char c2[9] = { "-avalueA" };
    char c3[9] = { "-bvalueB" };
    char c4[9] = { "-cvalueC" };
    char* const argv[] = { c1, c2, c3, c4 };
    const size_t argc = ARRAY_SIZE(argv);

    OptionValue<std::string> optionA('a', "aaaa", "an argument",
           "Just an argument", true);
    OptionValue<std::string> optionB('b', "bbbb", "an argument",
           "Just an argument", true);
    OptionValue<std::string> optionC('c', "cccc", "an argument",
           "Just an argument", true);

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);
    BOOST_CHECK_EQUAL(optionA.value(), "valueA");

    BOOST_CHECK(optionB.canParse(*(argv + 2)));
    nrTokensParsed = optionB.parse(argc - 2, argv + 2);
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);
    BOOST_CHECK_EQUAL(optionB.value(), "valueB");

    BOOST_CHECK(optionC.canParse(*(argv + 3)));
    nrTokensParsed = optionC.parse(argc - 3, argv + 3);
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionC.isParsed(), true);
    BOOST_CHECK_EQUAL(optionC.value(), "valueC");
  }
}



void com::CommandLineArgumentTest::testOptionList()
{
  {
    OptionList<std::string> optionA('a', "aaaa", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionB('b', "bbbb", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionC('c', "cccc", "some arguments",
           "Just some argument", true);

    BOOST_CHECK_EQUAL(optionA.isRequired(), true);
    BOOST_CHECK_EQUAL(optionB.isRequired(), true);
    BOOST_CHECK_EQUAL(optionC.isRequired(), true);
    BOOST_CHECK_EQUAL(optionA.isParsed(), false);
    BOOST_CHECK_EQUAL(optionB.isParsed(), false);
    BOOST_CHECK_EQUAL(optionC.isParsed(), false);
  }

  {
    char c1[4] = { "cmd" };
    char c2[3] = { "-a" };
    char c3[8] = { "valueA1" };
    char c4[8] = { "valueA2" };
    char c5[8] = { "valueA3" };
    char c6[3] = { "-b" };
    char c7[8] = { "valueB1" };
    char c8[3] = { "-c" };
    char c9[8] = { "valueC1" };
    char c10[8] = { "valueC2" };
    char* const argv[] = { c1, c2, c3, c4, c5, c6, c7, c8, c9, c10 };
    const size_t argc = ARRAY_SIZE(argv);

    OptionList<std::string> optionA('a', "aaaa", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionB('b', "bbbb", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionC('c', "cccc", "some arguments",
           "Just some argument", true);

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 4);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);
    BOOST_CHECK_EQUAL(optionA.size(), 3);
    BOOST_CHECK_EQUAL(optionA[0], "valueA1");
    BOOST_CHECK_EQUAL(optionA[1], "valueA2");
    BOOST_CHECK_EQUAL(optionA[2], "valueA3");

    BOOST_CHECK(optionB.canParse(*(argv + 5)));
    nrTokensParsed = optionB.parse(argc - 5, argv + 5);
    BOOST_CHECK_EQUAL(nrTokensParsed, 2);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);
    BOOST_CHECK_EQUAL(optionB.size(), 1);
    BOOST_CHECK_EQUAL(optionB[0], "valueB1");

    BOOST_CHECK(optionC.canParse(*(argv + 7)));
    nrTokensParsed = optionC.parse(argc - 7, argv + 7);
    BOOST_CHECK_EQUAL(nrTokensParsed, 3);
    BOOST_CHECK_EQUAL(optionC.isParsed(), true);
    BOOST_CHECK_EQUAL(optionC.size(), 2);
    BOOST_CHECK_EQUAL(optionC[0], "valueC1");
    BOOST_CHECK_EQUAL(optionC[1], "valueC2");
  }

  {
    char c1[4] = { "cmd" };
    char c2[10] = { "-avalueA1" };
    char c3[8] = { "valueA2" };
    char c4[8] = { "valueA3" };
    char c5[10] = { "-bvalueB1" };
    char c6[10] = { "-cvalueC1" };
    char c7[8] = { "valueC2" };
    char* const argv[] = { c1, c2, c3, c4, c5, c6, c7 };
    const size_t argc = ARRAY_SIZE(argv);

    OptionList<std::string> optionA('a', "aaaa", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionB('b', "bbbb", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionC('c', "cccc", "some arguments",
           "Just some argument", true);

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 3);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);
    BOOST_CHECK_EQUAL(optionA.size(), 3);
    BOOST_CHECK_EQUAL(optionA[0], "valueA1");
    BOOST_CHECK_EQUAL(optionA[1], "valueA2");
    BOOST_CHECK_EQUAL(optionA[2], "valueA3");

    BOOST_CHECK(optionB.canParse(*(argv + 4)));
    nrTokensParsed = optionB.parse(argc - 4, argv + 4);
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);
    BOOST_CHECK_EQUAL(optionB.size(), 1);
    BOOST_CHECK_EQUAL(optionB[0], "valueB1");

    BOOST_CHECK(optionC.canParse(*(argv + 5)));
    nrTokensParsed = optionC.parse(argc - 5, argv + 5);
    BOOST_CHECK_EQUAL(nrTokensParsed, 2);
    BOOST_CHECK_EQUAL(optionC.isParsed(), true);
    BOOST_CHECK_EQUAL(optionC.size(), 2);
    BOOST_CHECK_EQUAL(optionC[0], "valueC1");
    BOOST_CHECK_EQUAL(optionC[1], "valueC2");
  }


  {
    char c1[8] = "-a 1,2,";
    char* const arg = { c1 };

    OptionList<std::string> optionA('a', "aaaa", "some arguments",
           "Just some argument", true);

    optionA.setSeparator(',');

    // size_t nrTokensParsed;

    bool exceptionThrown = false;
    BOOST_CHECK(optionA.canParse(arg));
    try {
      optionA.parse(1, &arg);
    }
    catch(com::Exception const& exception) {
      exceptionThrown = true;
      // TODO Check error message, missing value.
    }
    BOOST_WARN(exceptionThrown);
  }
  {
    char c1[4] = { "cmd" };
    char c2[14] = { "-a v1, v2, v3" };
    char c3[12] = { "-b v4,v5,v6" };
    char* const argv[] = { c1, c2, c3 };
    const size_t argc = ARRAY_SIZE(argv);

    OptionList<std::string> optionA('a', "aaaa", "some arguments",
           "Just some argument", true);
    OptionList<std::string> optionB('b', "bbbb", "some arguments",
           "Just some argument", true);

    optionA.setSeparator(',');
    optionB.setSeparator(',');

    size_t nrTokensParsed;

    BOOST_CHECK(optionA.canParse(*(argv + 1)));
    nrTokensParsed = optionA.parse(argc - 1, argv + 1);
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionA.isParsed(), true);
    BOOST_CHECK_EQUAL(optionA.size(), 3);
    BOOST_CHECK_EQUAL(optionA[0], "v1");
    BOOST_CHECK_EQUAL(optionA[1], "v2");
    BOOST_CHECK_EQUAL(optionA[2], "v3");

    BOOST_CHECK(optionB.canParse(*(argv + 2)));
    nrTokensParsed = optionB.parse(argc - 2, argv + 2);
    BOOST_CHECK_EQUAL(nrTokensParsed, 1);
    BOOST_CHECK_EQUAL(optionB.size(), 3);
    BOOST_CHECK_EQUAL(optionB.isParsed(), true);
    BOOST_CHECK_EQUAL(optionB[0], "v4");
    BOOST_CHECK_EQUAL(optionB[1], "v5");
    BOOST_CHECK_EQUAL(optionB[2], "v6");
  }
}



void com::CommandLineArgumentTest::testArgumentSort()
{
  // Adding arguments also partly sorts them. Options come before the
  // Positionals, options are sorted on alphabet and positionals are arranged
  // according to the order in which they are added.

  Option optionA('a', "aaaa", "just an option", true);
  Option optionB('b', "bbbb", "just an option", true);
  Option optionC('c', "cccc", "just an option", true);

  PositionalValue<std::string> positional1("test argument 1",
         "Just a test argument", true);
  PositionalValue<std::string> positional2("test argument 2",
         "Just a test argument", true);

  CommandLineArguments::const_iterator iterator;

  // a, b, c -> a, b, c
  CommandLineArguments arguments1;
  arguments1.add(&positional1);
  arguments1.add(&optionA);
  arguments1.add(&optionB);
  arguments1.add(&positional2);
  arguments1.add(&optionC);

  iterator = arguments1.begin();
  BOOST_CHECK(*iterator++ == &optionA);
  BOOST_CHECK(*iterator++ == &optionB);
  BOOST_CHECK(*iterator++ == &optionC);
  BOOST_CHECK(*iterator++ == &positional1);
  BOOST_CHECK(*iterator++ == &positional2);

  // b, c, a -> a, b, c
  CommandLineArguments arguments2;
  arguments2.add(&positional1);
  arguments2.add(&optionA);
  arguments2.add(&optionB);
  arguments2.add(&positional2);
  arguments2.add(&optionC);

  iterator = arguments2.begin();
  BOOST_CHECK(*iterator++ == &optionA);
  BOOST_CHECK(*iterator++ == &optionB);
  BOOST_CHECK(*iterator++ == &optionC);
  BOOST_CHECK(*iterator++ == &positional1);
  BOOST_CHECK(*iterator++ == &positional2);

  // c, a, b -> a, b, c
  CommandLineArguments arguments3;
  arguments3.add(&positional1);
  arguments3.add(&optionA);
  arguments3.add(&optionB);
  arguments3.add(&positional2);
  arguments3.add(&optionC);

  iterator = arguments3.begin();
  BOOST_CHECK(*iterator++ == &optionA);
  BOOST_CHECK(*iterator++ == &optionB);
  BOOST_CHECK(*iterator++ == &optionC);
  BOOST_CHECK(*iterator++ == &positional1);
  BOOST_CHECK(*iterator++ == &positional2);

  // c, b, a -> a, b, c
  CommandLineArguments arguments4;
  arguments4.add(&positional1);
  arguments4.add(&optionA);
  arguments4.add(&optionB);
  arguments4.add(&positional2);
  arguments4.add(&optionC);

  iterator = arguments4.begin();
  BOOST_CHECK(*iterator++ == &optionA);
  BOOST_CHECK(*iterator++ == &optionB);
  BOOST_CHECK(*iterator++ == &optionC);
  BOOST_CHECK(*iterator++ == &positional1);
  BOOST_CHECK(*iterator++ == &positional2);
}



void com::CommandLineArgumentTest::testRepeatableArgument()
{
  char c1[4] = { "cmd" };
  char c2[5] = { "arg1" };
  char c3[5] = { "arg2" };
  char c4[5] = { "arg3" };
  char* const argv[] = { c1, c2, c3, c4 };
  const size_t argc = ARRAY_SIZE(argv);

  // This is where the parsed arguments end up.
  typedef PositionalValue<std::string> ArgType;
  typedef std::vector<ArgType> ColType;

  ColType arguments;
  RepeatableArgument<ArgType> repArg(
                   "repeatable test argument", true,
                   ArgType("test argument", "Just a test argument", true),
                   std::back_inserter(arguments));

  BOOST_CHECK_EQUAL(repArg.isRequired(), true);
  BOOST_CHECK_EQUAL(repArg.isParsed(), false);

  size_t nrTokensParsed;

  nrTokensParsed = repArg.parse(argc - 1, argv + 1);
  BOOST_CHECK_EQUAL(nrTokensParsed, 3);
  BOOST_CHECK_EQUAL(repArg.isParsed(), true);
  BOOST_CHECK_EQUAL(arguments.size(), 3);
  BOOST_CHECK_EQUAL(arguments[0].value(), "arg1");
  BOOST_CHECK_EQUAL(arguments[1].value(), "arg2");
  BOOST_CHECK_EQUAL(arguments[2].value(), "arg3");
}



void com::CommandLineArgumentTest::testExclusiveArgument()
{
  char c1[4] = { "cmd" };
  char c2[3] = { "-b" };
  char c3[5] = { "arg2" };
  char c4[5] = { "arg3" };
  char* const argv[] = { c1, c2, c3, c4 };
  const size_t argc = ARRAY_SIZE(argv);

  Option optionA('a', "aaaa", "just an option", true);
  Option optionB('b', "bbbb", "just an option", true);

  ExclusiveArgument<Option> exclArg("exclusive test argument", true);
  exclArg.addArgument(&optionA);
  exclArg.addArgument(&optionB);

  BOOST_CHECK_EQUAL(exclArg.isRequired(), true);
  BOOST_CHECK_EQUAL(exclArg.isParsed(), false);

  size_t nrTokensParsed = exclArg.parse(argc - 1, argv + 1);
  BOOST_CHECK_EQUAL(nrTokensParsed, 1);
  BOOST_CHECK_EQUAL(exclArg.isParsed(), true);
  BOOST_CHECK_EQUAL(optionA.isParsed(), false);
  BOOST_CHECK_EQUAL(optionB.isParsed(), true);
}



void com::CommandLineArgumentTest::testRepeatableExclusiveArgument()
{
  char c1[4] = { "cmd" };
  char c2[3] = { "-a" };
  char c3[4] = { "aaa" };
  char c4[3] = { "-b" };
  char c5[4] = { "bbb" };
  char c6[3] = { "-c" };
  char c7[4] = { "ccc" };
  char* const argv[] = { c1, c2, c3, c4, c5, c6, c7 };
  const size_t argc = ARRAY_SIZE(argv);

  // This is where the parsed arguments end up.
  typedef OptionValue<std::string> ArgType;
  typedef std::vector<ArgType> ColType;

  ColType argumentsA;
  ColType argumentsB;
  ColType argumentsC;

  RepeatableExclusiveArgument<ArgType> repExclArg(
                   "repeatable exclusive test argument", true,
                   ArgType('a', "bla", "a argument", "Just a a argument", true),
                   std::back_inserter(argumentsA),
                   ArgType('b', "bli", "b argument", "Just a b argument", true),
                   std::back_inserter(argumentsB),
                   ArgType('c', "blo", "c argument", "Just a c argument", true),
                   std::back_inserter(argumentsC));

  BOOST_CHECK_EQUAL(repExclArg.isRequired(), true);
  BOOST_CHECK_EQUAL(repExclArg.isParsed(), false);

  size_t nrTokensParsed;

  nrTokensParsed = repExclArg.parse(argc - 1, argv + 1);
  BOOST_CHECK_EQUAL(nrTokensParsed, argc - 1);
  BOOST_CHECK_EQUAL(repExclArg.isParsed(), true);
  BOOST_CHECK_EQUAL(argumentsA.size(), 1);
  BOOST_CHECK_EQUAL(argumentsA[0].value(), "aaa");
  BOOST_CHECK_EQUAL(argumentsB.size(), 1);
  BOOST_CHECK_EQUAL(argumentsB[0].value(), "bbb");
  BOOST_CHECK_EQUAL(argumentsC.size(), 1);
  BOOST_CHECK_EQUAL(argumentsC[0].value(), "ccc");
}



