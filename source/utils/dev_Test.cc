#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATIONTEST
#include "dev_CommandLineApplicationTest.h"
#define INCLUDED_DEV_COMMANDLINEAPPLICATIONTEST
#endif

#ifndef INCLUDED_DEV_QTCLIENTTEST
#include "dev_QtClientTest.h"
#define INCLUDED_DEV_QTCLIENTTEST
#endif

#ifndef INCLUDED_DEV_UTILSTEST
#include "dev_UtilsTest.h"
#define INCLUDED_DEV_UTILSTEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(
         int argc,
         char** const argv) {

  struct TestSuite: public boost::unit_test::test_suite
  {
    TestSuite(
         int& /* argc */,
         char** /* argv */)
      : boost::unit_test::test_suite("Master test suite")
    {
    }
  };

  TestSuite* test = new TestSuite(argc, argv);

  test->add(dev::UtilsTest::suite());
  test->add(dev::CommandLineApplicationTest::suite());
  test->add(dev::QtClientTest::suite());

  return test;
}

