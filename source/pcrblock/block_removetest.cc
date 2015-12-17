#define BOOST_TEST_MODULE pcrblock remove
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "discr_raster.h"
#include "discr_rasterdata.h"
#include "block_compactors.h"
#include "block_dummycompactor.h"
#include "block_functions.h"


struct Fixture
{

    Fixture()
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
      block::Compactors<block::MackeyBridgeCompactor> compactors;
      compactors.setCompactor(3, block::DummyCompactor());

      // TODO work around this add, see ResampleTest, remove functions.h inclusion.
      mackeyBridgeAdd(*d_block, *d_originalThickness, *d_sediment, thickness,
             d_maxVoxelThickness, compactors);
    }

    ~Fixture()
    {
      delete d_sediment;
      delete d_originalThickness;
      delete d_block;
      delete d_raster;
    }

    discr::Raster* d_raster;

    discr::Block* d_block;

    discr::BlockData<REAL4>* d_originalThickness;

    discr::BlockData<INT4>* d_sediment;

    REAL4 d_maxVoxelThickness;

};


BOOST_FIXTURE_TEST_SUITE(remove_, Fixture)

BOOST_AUTO_TEST_CASE(remove__)
{
  using namespace block;

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
}


BOOST_AUTO_TEST_CASE(remove_more_than_available)
{
  using namespace block;

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
}

BOOST_AUTO_TEST_SUITE_END()
