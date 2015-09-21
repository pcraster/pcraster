#define BOOST_TEST_MODULE string utils test suite
#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_DEV_TOSTRINGTEST
#include "dev_ToStringTest.h"
#define INCLUDED_DEV_TOSTRINGTEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(
         int /* argc */,
         char** const /* argv */) {

  struct TestSuite: public boost::unit_test::test_suite
  {
    TestSuite()
      : boost::unit_test::test_suite("Master test suite")
    {
    }

    ~TestSuite() {
    }
  };

  TestSuite* test = new TestSuite();

  test->add(dev::ToStringTest().suite());

  return test;
}

