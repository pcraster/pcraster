#define BOOST_TEST_MODULE math_utils test suite
#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif


#ifndef INCLUDED_DEV_MATHUTILSTEST
#include "dev_MathUtilsTest.h"
#define INCLUDED_DEV_MATHUTILSTEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(
         int /* argc */,
         char** const /* argv */) {
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE("Master test suite");

  test->add(dev::MathUtilsTest().suite());

  return test;
}
