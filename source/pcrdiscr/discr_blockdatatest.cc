#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_BLOCKDATATEST
#include "discr_blockdatatest.h"
#define INCLUDED_DISCR_BLOCKDATATEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif



/*!
  \file
  This file contains the implementation of the BlockDataTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCKDATA MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*BlockDataTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BlockDataTest> instance(new BlockDataTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BlockDataTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockDataTest::testSetDefaultValue, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockDataTest::testAddVoxels, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockDataTest::testRemoveVoxels, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockDataTest::testCutVoxels, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BLOCKDATA MEMBERS
//------------------------------------------------------------------------------

//! ctor
BlockDataTest::BlockDataTest(
         )
{
}



//! setUp
void BlockDataTest::setUp()
{
}



//! tearDown
void BlockDataTest::tearDown()
{
}



void BlockDataTest::testConstructor()
{
  size_t nrRows = 2;
  size_t nrCols = 1;
  double cellSize = 1.0;
  double west = 1.0;
  double north = 0.0;
  Raster raster(nrRows, nrCols, cellSize, west, north);
  Block block(raster);

  {
    Block block(raster);
    BlockData<INT4> sediment(&block);

    for(size_t i = 0; i < sediment.block()->nrCells(); ++i) {
      BOOST_CHECK(sediment.cell(i).size() == 0);
      BOOST_CHECK(!sediment.block()->cell(i).isMV());
      BOOST_CHECK(sediment.block()->cell(i).size() == 0);
      BOOST_CHECK(pcr::isMV(sediment.defaultValue().cell(i)));
    }
  }

  {
    Block block(raster);
    BlockData<INT4> sediment(&block, 3);

    for(size_t i = 0; i < sediment.block()->nrCells(); ++i) {
      BOOST_CHECK(sediment.cell(i).size() == 0);
      BOOST_CHECK(!sediment.block()->cell(i).isMV());
      BOOST_CHECK(sediment.block()->cell(i).size() == 0);
      BOOST_CHECK(sediment.defaultValue().cell(i) == 3);
    }
  }

  {
    Block block(raster);
    RasterData<INT4> defaultValues(&raster);
    defaultValues.cell(0) = 3;
    defaultValues.cell(1) = 5;
    BlockData<INT4> sediment(&block, defaultValues);

    for(size_t i = 0; i < sediment.block()->nrCells(); ++i) {
      BOOST_CHECK(sediment.cell(i).size() == 0);
      BOOST_CHECK(!sediment.block()->cell(i).isMV());
      BOOST_CHECK(sediment.block()->cell(i).size() == 0);
    }

    BOOST_CHECK(sediment.defaultValue().cell(0) == 3);
    BOOST_CHECK(sediment.defaultValue().cell(1) == 5);
  }
}



void BlockDataTest::testSetDefaultValue()
{
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  RasterData<REAL4> baseElevation(&raster, 5.0);
  Block block(baseElevation);

  {
    BlockData<INT4> sediment(&block, 3);
    sediment.setDefaultValue(4);

    for(size_t i = 0; i < block.nrCells(); ++i) {
      BOOST_CHECK(!sediment.block()->cell(i).isMV());
      BOOST_CHECK(sediment.defaultValue().cell(i) == 4);
    }
  }
}



void BlockDataTest::testAddVoxels()
{
  Raster raster(2, 1, 1.0, 0.0, 0.0);

  {
    Block block(raster, 5.0);
    BlockData<INT4> sediment(&block, 3);
    BOOST_CHECK(sediment.cell(0).empty());

    block.addVoxels(1, 2.0);
    BOOST_CHECK(sediment.cell(0).size() == 1);
    BOOST_CHECK(sediment.cell(0)[0] == 3);
  }

  {
    Block block(raster, 5.0);
    RasterData<INT4> defaultValues(&raster);
    pcr::setMV(defaultValues.cell(0));
    defaultValues.cell(1) = 7;
    BlockData<INT4> sediment(&block, defaultValues);
    BOOST_CHECK(sediment.cell(0).empty());

    block.addVoxels(1, 2.0);
    BOOST_CHECK(sediment.cell(0).size() == 1);
    BOOST_CHECK(pcr::isMV(sediment.cell(0)[0]));
    BOOST_CHECK(sediment.cell(1).size() == 1);
    BOOST_CHECK(sediment.cell(1)[0] == 7);
  }
}



void BlockDataTest::testRemoveVoxels()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



void BlockDataTest::testCutVoxels()
{
  // Test for all supported value types. Cut behaviour differs per value type.
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace discr

