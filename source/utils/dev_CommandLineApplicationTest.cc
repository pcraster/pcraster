#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATIONTEST
#include "dev_CommandLineApplicationTest.h"
#define INCLUDED_DEV_COMMANDLINEAPPLICATIONTEST
#endif

// External headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

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

// Project headers.

// Module headers.
#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATION
#include "dev_CommandLineApplication.h"
#define INCLUDED_DEV_COMMANDLINEAPPLICATION
#endif



/*!
  \file
  This file contains the implementation of the CommandLineApplicationTest class.
*/



namespace dev {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINEAPPLICATIONTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* CommandLineApplicationTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CommandLineApplicationTest> instance(
         new CommandLineApplicationTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &CommandLineApplicationTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINEAPPLICATIONTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
CommandLineApplicationTest::CommandLineApplicationTest()
{
}



void CommandLineApplicationTest::test()
{
  namespace po = boost::program_options;

  {
    char arg0[8] = "goforit";
    char* argv[] = { arg0 };
    unsigned short int argc = 1;
    CommandLineApplication application(argc, argv);

    application.genericOptions().add_options()
         ("aaa", "aaa aaa aaa")
         ("bbb", "bbb bbb bbb");
    application.hiddenOptions().add_options()
         ("ccc", po::value<std::vector<std::string> >());
    application.addPositionalOption(
         "ccc", -1, "ccc ccc ccc");

    std::ostringstream stream;
    application.usage(stream);
    stream << std::ends;

    std::ostringstream result;
    result <<
         "goforit options ccc ...\n"
         "\n"
         "options:\n"
         "  --help                Produce help message.\n"
         "  --version             Show version.\n"
         "  --aaa                 aaa aaa aaa\n"
         "  --bbb                 bbb bbb bbb\n"
         "\n"
         "ccc: ccc ccc ccc\n"
         ;
    result << std::ends;

    BOOST_CHECK_EQUAL(stream.str(), result.str());
  }
}

} // namespace dev

