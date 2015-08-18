#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_MLDD_DAGRASTERTEST
#include "mldd_dagrastertest.h"
#define INCLUDED_MLDD_DAGRASTERTEST
#endif
#ifndef INCLUDED_MLDD_OUTEDGEITERATORTEST
#include "mldd_outedgeiteratortest.h"
#define INCLUDED_MLDD_OUTEDGEITERATORTEST
#endif
#ifndef INCLUDED_MLDD_VERTEXITERATORTEST
#include "mldd_vertexiteratortest.h"
#define INCLUDED_MLDD_VERTEXITERATORTEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(
  int /* argc */,
  char** const /* argv */)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(mldd::OutEdgeIteratorTest::suite());
  test->add(mldd::VertexIteratorTest().suite());
  test->add(mldd::DagRasterTest().suite());

  return test;
}

