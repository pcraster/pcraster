#define BOOST_TEST_MODULE modflow_calc test suite
#include "stddefx.h"



#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_CALC_DEMOTEST
#include "calc_DemoTest.h"
#define INCLUDED_CALC_DEMOTEST
#endif

boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(calc::DemoTest().suite());

  return test;
}
