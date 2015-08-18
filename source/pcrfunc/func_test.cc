#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

#ifndef INCLUDED_FUNC_ASSIGNTEST
#include "func_assigntest.h"
#define INCLUDED_FUNC_ASSIGNTEST
#endif

#ifndef INCLUDED_FUNC_BOOLEANOPERATORSTEST
#include "func_booleanoperatorstest.h"
#define INCLUDED_FUNC_BOOLEANOPERATORSTEST
#endif

#ifndef INCLUDED_FUNC_SETMVTEST
#include "func_setmvtest.h"
#define INCLUDED_FUNC_SETMVTEST
#endif

#ifndef INCLUDED_FUNC_SUMMARYSTATISTICSTEST
#include "func_summarystatisticstest.h"
#define INCLUDED_FUNC_SUMMARYSTATISTICSTEST
#endif





boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/) {

  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(func::SetMVTest().suite());
  test->add(func::AssignTest().suite());
  test->add(func::BooleanOperatorsTest().suite());
  test->add(func::SummaryStatisticsTest().suite());

  return test;
}


