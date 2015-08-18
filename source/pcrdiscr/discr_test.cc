#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif


#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

#ifndef INCLUDED_DISCR_BLOCKTEST
#include "discr_blocktest.h"
#define INCLUDED_DISCR_BLOCKTEST
#endif

#ifndef INCLUDED_DISCR_BLOCKDATATEST
#include "discr_blockdatatest.h"
#define INCLUDED_DISCR_BLOCKDATATEST
#endif

#ifndef INCLUDED_DISCR_RASTERDATATEST
#include "discr_rasterdatatest.h"
#define INCLUDED_DISCR_RASTERDATATEST
#endif

#ifndef INCLUDED_DISCR_RASTERTEST
#include "discr_rastertest.h"
#define INCLUDED_DISCR_RASTERTEST
#endif

#ifndef INCLUDED_DISCR_VOXELSTACKTEST
#include "discr_voxelstacktest.h"
#define INCLUDED_DISCR_VOXELSTACKTEST
#endif





boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/) {

  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(discr::RasterTest().suite());
  test->add(discr::RasterDataTest().suite());

  test->add(discr::VoxelStackTest().suite());

  test->add(discr::BlockTest().suite());
  test->add(discr::BlockDataTest().suite());

  return test;
}


