#define BOOST_TEST_MODULE pt test suite
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_PT_PARTICLETEST
#include "pt_ParticleTest.h"
#define INCLUDED_PT_PARTICLETEST
#endif


boost::unit_test::test_suite* init_unit_test_suite(
         int ,
         char ** const )
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(pt::ParticleTest().suite());

  return test;
}

