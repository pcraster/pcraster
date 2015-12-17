#define BOOST_TEST_MODULE pcrblock profile
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "block_functions.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace block;

  // Create a block.
  discr::Raster raster(3, 2);
  discr::Block block(raster);

  // Create an attribute.
  discr::BlockData<REAL4> date(&block);

  size_t lastTimeStep = 10000;
  REAL4 thickness = 1.0;

  // Fill the block/attribute.
  discr::RasterData<REAL4> value(&raster);

  for(size_t i = 1; i <= lastTimeStep; ++i) {
    for(size_t j = 0; j < raster.nrCells(); ++j) {
      value.cell(j) = i + j;
    }

    date.setDefaultValue(value);
    block.addVoxels(1, thickness);
  }

  // Determine profiles at different heights and test the contents.
  discr::RasterData<REAL4> result(&raster);

  for(size_t i = 1; i <= lastTimeStep; ++i) {
    // Bottom of voxel.
    profile(result, date, (i - 1) * thickness);

    for(size_t j = 0; j < raster.nrCells(); ++j) {
      BOOST_CHECK(dal::comparable(result.cell(j), REAL4(i + j)));
    }

    // Center of voxel.
    profile(result, date, (i - 1) * thickness + 0.5 * thickness);
    BOOST_CHECK(dal::comparable(result.cell(0), REAL4(i)));

    for(size_t j = 0; j < raster.nrCells(); ++j) {
      BOOST_CHECK(dal::comparable(result.cell(j), REAL4(i + j)));
    }

    // Top of voxel == bottom of upper voxel, except for the upper voxel.
    profile(result, date, i * thickness);

    if(i != lastTimeStep) {
      for(size_t j = 0; j < raster.nrCells(); ++j) {
        BOOST_CHECK(dal::comparable(result.cell(j), REAL4(i + j + 1)));
      }
    }
    else {
      for(size_t j = 0; j < raster.nrCells(); ++j) {
        BOOST_CHECK(pcr::isMV(result.cell(j)));
      }
    }
  }
}
