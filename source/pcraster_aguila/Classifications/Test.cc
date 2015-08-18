#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_RANGECLASSIFICATIONTEST
#include "RangeClassificationTest.h"
#define INCLUDED_RANGECLASSIFICATIONTEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(
         int /* argc */,
         char** /* argv */) {

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

  test->add(ag::RangeClassificationTest::suite());

  return test;
}

