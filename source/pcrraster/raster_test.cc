#define BOOST_TEST_MODULE raster test suite
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

#ifndef INCLUDED_RASTER_IOTEST
#include "raster_iotest.h"
#define INCLUDED_RASTER_IOTEST
#endif

#ifndef INCLUDED_RASTER_RASTERTEST
#include "raster_rastertest.h"
#define INCLUDED_RASTER_RASTERTEST
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif





boost::unit_test::test_suite* init_unit_test_suite(int /* argc */, char ** const /*argv*/) {

  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(raster::RasterTest().suite());
  test->add(raster::IOTest().suite());

  return test;
}


