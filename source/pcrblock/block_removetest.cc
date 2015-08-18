#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_REMOVETEST
#include "block_removetest.h"
#define INCLUDED_BLOCK_REMOVETEST
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
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

// Module headers.
#ifndef INCLUDED_BLOCK_COMPACTORS
#include "block_compactors.h"
#define INCLUDED_BLOCK_COMPACTORS
#endif

#ifndef INCLUDED_BLOCK_DUMMYCOMPACTOR
#include "block_dummycompactor.h"
#define INCLUDED_BLOCK_DUMMYCOMPACTOR
#endif

#ifndef INCLUDED_BLOCK_FUNCTIONS
#include "block_functions.h"
#define INCLUDED_BLOCK_FUNCTIONS
#endif



/*!
  \file
  This file contains the implementation of the RemoveTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC REMOVE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*RemoveTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RemoveTest> instance(new RemoveTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RemoveTest::testRemove, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RemoveTest::testRemoveMoreThanAvailable, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF REMOVE MEMBERS
//------------------------------------------------------------------------------

//! ctor
RemoveTest::RemoveTest(
         )
{
}



//! setUp
void RemoveTest::setUp()
{
  // Set up a block with some voxels and a blockdata filled with sediment.
  size_t nrRows = 3;
  size_t nrCols = 4;
  double cellSize = 1.5;
  double west = 1.0;
  double north = 0.0;

  // Discretisations.
  d_raster = new discr::Raster(nrRows, nrCols, cellSize, west, north);
  discr::RasterData<REAL4> baseElevation(d_raster, REAL4(5.0));
  d_block = new discr::Block(baseElevation);

  // Data.
  d_originalThickness = new discr::BlockData<REAL4>(d_block);
  d_sediment = new discr::BlockData<INT4>(d_block, 3);

  // Add function arguments.
  discr::RasterData<REAL4> thickness(d_raster, REAL4(15.5));
  d_maxVoxelThickness = REAL4(1.0);

  // No compaction.
  block::Compactors<MackeyBridgeCompactor> compactors;
  compactors.setCompactor(3, block::DummyCompactor());

  // TODO work around this add, see ResampleTest, remove functions.h inclusion.
  mackeyBridgeAdd(*d_block, *d_originalThickness, *d_sediment, thickness,
         d_maxVoxelThickness, compactors);
}



//! tearDown
void RemoveTest::tearDown()
{
  delete d_sediment;
  delete d_originalThickness;
  delete d_block;
  delete d_raster;
}



void RemoveTest::testRemove()
{
  setUp();

  // Remove nothing.
  {
    discr::RasterData<REAL4> thickness(d_raster, REAL4(0.0));
    remove(*d_block, thickness);

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      BOOST_CHECK(d_block->cell(i).size() == 16);
      BOOST_CHECK(d_block->cell(i).thickness() == REAL4(15.5));
      BOOST_CHECK(d_block->cell(i).back() == REAL4(0.5));
    }

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      for(size_t j = 0; j < d_block->cell(i).size() - 1; ++j) {
        BOOST_CHECK(d_originalThickness->cell(i)[j] == d_maxVoxelThickness);
        BOOST_CHECK(d_sediment->cell(i)[j] == 3);
      }

      BOOST_CHECK(d_originalThickness->cell(i).back() == REAL4(0.5));
      BOOST_CHECK(d_sediment->cell(i).back() == 3);
    }
  }

  // Remove a little.
  {
    discr::RasterData<REAL4> thickness(d_raster, REAL4(0.4));
    remove(*d_block, thickness);

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      BOOST_CHECK(d_block->cell(i).size() == 16);
      BOOST_CHECK(dal::comparable(d_block->cell(i).thickness(), REAL4(15.1)));
      BOOST_CHECK(dal::comparable(d_block->cell(i).back(), REAL4(0.1)));
    }

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      for(size_t j = 0; j < d_block->cell(i).size() - 1; ++j) {
        BOOST_CHECK(dal::comparable(d_originalThickness->cell(i)[j],
              d_maxVoxelThickness));
        BOOST_CHECK(d_sediment->cell(i)[j] == 3);
      }

      BOOST_CHECK(dal::comparable(
              d_originalThickness->cell(i).back(), REAL4(0.5)));
      BOOST_CHECK(d_sediment->cell(i).back() == 3);
    }
  }

  // Remove some more.
  {
    discr::RasterData<REAL4> thickness(d_raster, REAL4(0.4));
    remove(*d_block, thickness);

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      BOOST_CHECK(d_block->cell(i).size() == 15);
      BOOST_CHECK(dal::comparable(d_block->cell(i).thickness(), REAL4(14.7)));
      BOOST_CHECK(dal::comparable(d_block->cell(i).back(), REAL4(0.7)));
    }

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      for(size_t j = 0; j < d_block->cell(i).size() - 1; ++j) {
        BOOST_CHECK(dal::comparable(d_originalThickness->cell(i)[j],
              d_maxVoxelThickness));
        BOOST_CHECK(d_sediment->cell(i)[j] == 3);
      }

      BOOST_CHECK(dal::comparable(
              d_originalThickness->cell(i).back(), REAL4(1.0)));
      BOOST_CHECK(d_sediment->cell(i).back() == 3);
    }
  }

  // Remove still some more.
  {
    discr::RasterData<REAL4> thickness(d_raster, REAL4(4.4));
    remove(*d_block, thickness);

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      BOOST_CHECK(d_block->cell(i).size() == 11);
      BOOST_CHECK(dal::comparable(d_block->cell(i).thickness(), REAL4(10.3)));
      BOOST_CHECK(dal::comparable(d_block->cell(i).back(), REAL4(0.3)));
    }

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      for(size_t j = 0; j < d_block->cell(i).size() - 1; ++j) {
        BOOST_CHECK(dal::comparable(d_originalThickness->cell(i)[j],
              d_maxVoxelThickness));
        BOOST_CHECK(d_sediment->cell(i)[j] == 3);
      }

      BOOST_CHECK(dal::comparable(
              d_originalThickness->cell(i).back(), REAL4(1.0)));
      BOOST_CHECK(d_sediment->cell(i).back() == 3);
    }
  }

  BOOST_WARN(false);

  tearDown();
}



void RemoveTest::testRemoveMoreThanAvailable()
{
  setUp();

  // Remove more than available (dig in).
  {
    discr::RasterData<REAL4> thickness(d_raster, REAL4(16.0));
    remove(*d_block, thickness);

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      BOOST_CHECK(dal::comparable(d_block->cell(i).baseElevation(), REAL4(4.5)));
      BOOST_CHECK(d_block->cell(i).size() == 0);
      BOOST_CHECK(dal::comparable(d_block->cell(i).thickness(), REAL4(0)));
    }

    for(size_t i = 0; i < d_block->nrCells(); ++i) {
      BOOST_CHECK(d_originalThickness->cell(i).empty());
      BOOST_CHECK(d_sediment->cell(i).empty());
    }
  }

  tearDown();
}



} // namespace block

