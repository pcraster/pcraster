#define BOOST_TEST_MODULE pcraster discr block_data
#include <boost/test/unit_test.hpp>
#include "discr_block.h"
#include "discr_raster.h"
#include "discr_blockdata.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace discr;

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


BOOST_AUTO_TEST_CASE(set_default_value)
{
  using namespace discr;

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


BOOST_AUTO_TEST_CASE(add_voxels)
{
  using namespace discr;

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


BOOST_AUTO_TEST_CASE(remove_voxels)
{
  using namespace discr;

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(cut_voxels)
{
  using namespace discr;

  // Test for all supported value types. Cut behaviour differs per value type.
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}
