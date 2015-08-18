#include "stddefx.h"


#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_GEOM_POINTTEST
#include "geom_pointtest.h"
#define INCLUDED_GEOM_POINTTEST
#endif

/*!
 * \namespace geom
 * \brief geometric classes
 */

boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(geom::PointTest().suite());

  return  test;
}

