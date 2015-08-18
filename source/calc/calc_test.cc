#include "stddefx.h"



#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_CALC_KINEMATICTEST
#include "calc_kinematictest.h"
#define INCLUDED_CALC_KINEMATICTEST
#endif
#ifndef INCLUDED_CALC_TRANSIENTTEST
#include "calc_transienttest.h"
#define INCLUDED_CALC_TRANSIENTTEST
#endif
#ifndef INCLUDED_CALC_DOWNSTREAMVISITORTEST
#include "calc_downstreamvisitortest.h"
#define INCLUDED_CALC_DOWNSTREAMVISITORTEST
#endif
#ifndef INCLUDED_CALC_MARKTEST
#include "calc_marktest.h"
#define INCLUDED_CALC_MARKTEST
#endif

/*!
   \namespace calc
   \brief global field functions of pcrcalc

   Calc is a bad name for the namespace;
   In the long run, merge this lib and api, to be named globfieldfunc ??
*/

boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(calc::DownStreamVisitorTest().suite());
  test->add(calc::KinematicTest().suite());
  test->add(calc::TransientTest().suite());
  test->add(calc::MarkTest().suite());

  return test;
}

