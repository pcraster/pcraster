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

#ifndef INCLUDED_BLOCK_ADDTEST
#include "block_addtest.h"
#define INCLUDED_BLOCK_ADDTEST
#endif

#ifndef INCLUDED_BLOCK_ARITHMETICOPERATORSTEST
#include "block_arithmeticoperatorstest.h"
#define INCLUDED_BLOCK_ARITHMETICOPERATORSTEST
#endif

#ifndef INCLUDED_BLOCK_BLOCKTEST
#include "block_blocktest.h"
#define INCLUDED_BLOCK_BLOCKTEST
#endif

#ifndef INCLUDED_BLOCK_CASTTEST
#include "block_casttest.h"
#define INCLUDED_BLOCK_CASTTEST
#endif

#ifndef INCLUDED_BLOCK_COMPARETEST
#include "block_comparetest.h"
#define INCLUDED_BLOCK_COMPARETEST
#endif

#ifndef INCLUDED_BLOCK_COMPACTORSTEST
#include "block_compactorstest.h"
#define INCLUDED_BLOCK_COMPACTORSTEST
#endif

#ifndef INCLUDED_BLOCK_DEHAANCOMPACTORTEST
#include "block_dehaancompactortest.h"
#define INCLUDED_BLOCK_DEHAANCOMPACTORTEST
#endif

#ifndef INCLUDED_BLOCK_IOTEST
#include "block_iotest.h"
#define INCLUDED_BLOCK_IOTEST
#endif

#ifndef INCLUDED_BLOCK_MACKEYBRIDGECOMPACTORTEST
#include "block_mackeybridgecompactortest.h"
#define INCLUDED_BLOCK_MACKEYBRIDGECOMPACTORTEST
#endif

#ifndef INCLUDED_BLOCK_PROFILETEST
#include "block_profiletest.h"
#define INCLUDED_BLOCK_PROFILETEST
#endif

#ifndef INCLUDED_BLOCK_REMOVETEST
#include "block_removetest.h"
#define INCLUDED_BLOCK_REMOVETEST
#endif

#ifndef INCLUDED_BLOCK_RESAMPLETEST
#include "block_resampletest.h"
#define INCLUDED_BLOCK_RESAMPLETEST
#endif

#ifndef INCLUDED_BLOCK_VOXELATHEIGHTTEST
#include "block_voxelatheighttest.h"
#define INCLUDED_BLOCK_VOXELATHEIGHTTEST
#endif





boost::unit_test::test_suite* init_unit_test_suite(int /* argc*/, char ** const /*argv*/) {

  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(block::MackeyBridgeCompactorTest().suite());
  test->add(block::DeHaanCompactorTest().suite());
  test->add(block::CompactorsTest().suite());

  test->add(block::BlockTest().suite());
  test->add(block::VoxelAtHeightTest().suite());

  test->add(block::CastTest().suite());
  test->add(block::ArithmeticOperatorsTest().suite());
  test->add(block::CompareTest().suite());
  // RemoveTest uses add function. First test add.
  test->add(block::AddTest().suite());
  test->add(block::RemoveTest().suite());

  test->add(block::ResampleTest().suite());
  test->add(block::IOTest().suite());
  test->add(block::ProfileTest().suite());

  return test;
}


