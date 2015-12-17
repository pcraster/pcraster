#define BOOST_TEST_MODULE pcrblock resample
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "discr_raster.h"
#include "block_functions.h"


BOOST_AUTO_TEST_CASE(resample_block)
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(resample_block_data)
{
  using namespace block;

  size_t nrRows = 1;
  size_t nrCols = 1;
  double cellSize = 1.5;
  double west = 0.0;
  double north = 0.0;

  discr::Raster raster(nrRows, nrCols, cellSize, west, north);

  //////////////////////////////////////////////////////////////////////////////
  // TESTS WITH EMPTY BLOCKS AND BLOCKS WHICH ARE NOT AT THE SAME HEIGHT
  //////////////////////////////////////////////////////////////////////////////

  //----------------------------------------------------------------------------
  // Test  : New block lower than current block.
  //
  //         +-----+
  //         |     |
  //         |     |
  //         +-----+
  //                  ->
  //                      +-----+
  //                      |     |
  //                      |     |
  //                      +-----+
  //
  // Result: All missing values.
  {
    discr::Block block(raster, 5.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);
    block.addVoxels(1, 5.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : New block higher than current block.
  //
  //                      +-----+
  //                      |     |
  //                      |     |
  //                      +-----+
  //                  ->
  //         +-----+
  //         |     |
  //         |     |
  //         +-----+
  //
  // Result: All missing values.
  {
    discr::Block block(raster, -10.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);
    block.addVoxels(1, 5.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : New block lower than current block. Current block empty.
  //
  //         +-----+
  //                  ->
  //                      +-----+
  //                      |     |
  //                      |     |
  //                      +-----+
  //
  // Result: All missing values.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);

    discr::Block newBlock(raster, -10.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : New block higher than current block. Current block empty.
  //
  //                      +-----+
  //                      |     |
  //                      |     |
  //                      +-----+
  //                  ->
  //         +-----+
  //
  // Result: All missing values.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);

    discr::Block newBlock(raster, 10.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : New block at same hight as current block. Current block empty.
  //
  //                      +-----+
  //                      |     |
  //         +-----+  ->  |     |
  //                      +-----+
  //
  // Result: All missing values.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);

    discr::Block newBlock(raster, -1.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : New block higher than current block. New block empty.
  //
  //                      +-----+
  //                  ->
  //         +-----+
  //         |     |
  //         |     |
  //         +-----+
  //
  // Result: New block empty.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);
    block.addVoxels(1, 2.0);

    discr::Block newBlock(raster, 10.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(newSand.cell(0).empty());
    BOOST_CHECK(newSediment.cell(0).empty());
    BOOST_CHECK(newConcentration.cell(0).empty());
  }

  //----------------------------------------------------------------------------
  // Test  : New block lower than current block. New block empty.
  //
  //         +-----+
  //         |     |
  //         |     |
  //         +-----+
  //                  ->
  //                      +-----+
  //
  // Result: New block empty.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);
    block.addVoxels(1, 2.0);

    discr::Block newBlock(raster, -10.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(newSand.cell(0).empty());
    BOOST_CHECK(newSediment.cell(0).empty());
    BOOST_CHECK(newConcentration.cell(0).empty());
  }

  //----------------------------------------------------------------------------
  // Test  : New block at same height as current block. New block empty.
  //
  //         +-----+
  //         |     |
  //         |     |  ->  +-----+
  //         +-----+
  //
  // Result: New block empty.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);
    block.addVoxels(1, 2.0);

    discr::Block newBlock(raster, 1.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(newSand.cell(0).empty());
    BOOST_CHECK(newSediment.cell(0).empty());
    BOOST_CHECK(newConcentration.cell(0).empty());
  }

  //////////////////////////////////////////////////////////////////////////////
  // TESTS WITH BLOCKS AT THE SAME HEIGHT EN NO MISSING VALUES
  //////////////////////////////////////////////////////////////////////////////

  //----------------------------------------------------------------------------
  // Test  : newBlock has same discretization as block.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |     |  ->  |     |
  //         +-----+      +-----+
  //
  // Result: block ends up with the same discretization as before the
  //         the resample and with the same attribute values.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 3);
    discr::BlockData<REAL4> concentration(&block, 0.5);
    block.addVoxels(1, 2.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(0)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(3)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(0.5)));
  }

  //----------------------------------------------------------------------------
  // Test  : newBlock has same size/height as block. block has two
  //         equally sized voxels and newBlock has one.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |-----|  ->  |     |
  //         |     |      |     |
  //         +-----+      +-----+
  //
  // Result: block ends up with one, averaged, value for scalars
  //         first value for nominals
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 4);
    discr::BlockData<REAL4> concentration(&block, 1.0);
    block.addVoxels(1, 2.0);
    sand.setDefaultValue(1);
    sediment.setDefaultValue(5);
    concentration.setDefaultValue(2.0);
    block.addVoxels(1, 2.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 4.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(0)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(4)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(1.5)));
  }

  //----------------------------------------------------------------------------
  // Test  : newBlock has same size/height as block. block has two
  //         different sized voxels and newBlock has one.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |-----|  ->  |     |
  //         +-----+      +-----+
  //
  // Result: block ends up with one, averaged, value for scalars
  //         first value for nominals
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 4);
    discr::BlockData<REAL4> concentration(&block, 2.0);
    block.addVoxels(1, 1.0);
    sand.setDefaultValue(1);
    sediment.setDefaultValue(5);
    concentration.setDefaultValue(8.0);
    block.addVoxels(1, 2.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 3.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(1)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(5)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(6.0)));
  }

  //----------------------------------------------------------------------------
  // Test  : newBlock has same size/height as block. newBlock has two
  //         equally sized voxels and block has one.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |     |  ->  |-----|
  //         |     |      |     |
  //         +-----+      +-----+
  //
  // Result: block ends up with two, equal values.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 21);
    discr::BlockData<REAL4> concentration(&block, 5.0);
    block.addVoxels(1, 4.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(2, 2.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(0)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(21)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(5.0)));
  }

  //----------------------------------------------------------------------------
  // Test  : newBlock has same size/height as block. newBlock has two
  //         different sized voxels and block has one.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |     |  ->  |-----|
  //         +-----+      +-----+
  //
  // Result: block ends up with two, equal values.
  {
    discr::Block block(raster, 0.0);
    discr::BlockData<UINT1> sand(&block, 1);
    discr::BlockData<INT4> sediment(&block, 23);
    discr::BlockData<REAL4> concentration(&block, 9.0);
    block.addVoxels(1, 4.0);

    discr::Block newBlock(raster, 0.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 1.0);
    newBlock.addVoxels(1, 3.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(1)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(23)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(9.0)));
  }

  //----------------------------------------------------------------------------
  // Test  : block contains one cell whose base is positioned above half
  //         the height of newBlock. newBlock also contains an equally sized
  //         cell.
  //
  //         +-----+
  //         |     |
  //         |     |
  //         |     |  ->  +-----+
  //         +-----+      |     |
  //                      |     |
  //                      |     |
  //                      +-----+
  //
  // Result: block ends up with a missing value.
  {
    discr::Block block(raster, 2.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 33);
    discr::BlockData<REAL4> concentration(&block, 6.0);
    block.addVoxels(1, 1.0);

    discr::Block newBlock(raster, 1.4);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 1.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : block contains one cell whose base is positioned below half
  //         the height of newBlock. newBlock also contains an equally sized
  //         cell.
  //
  //         +-----+
  //         |     |  ->  +-----+
  //         |     |      |     |
  //         |     |      |     |
  //         +-----+      |     |
  //                      +-----+
  //
  // Result: block ends up with the same value as before.
  {
    discr::Block block(raster, 2.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 44);
    discr::BlockData<REAL4> concentration(&block, 7.0);
    block.addVoxels(1, 1.0);

    discr::Block newBlock(raster, 1.6);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 1.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(0)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(44)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(7.0)));
  }

  //----------------------------------------------------------------------------
  // Test  : block contains one cell whose top is positioned below half
  //         the height of newBlock. newBlock also contains an equally sized
  //         cell.
  //
  //                      +-----+
  //                      |     |
  //                      |     |
  //         +-----+      |     |
  //         |     |  ->  +-----+
  //         |     |
  //         |     |
  //         +-----+
  //
  // Result: block ends up with a missing value.
  {
    discr::Block block(raster, 1.4);
    discr::BlockData<UINT1> sand(&block, 1);
    discr::BlockData<INT4> sediment(&block, 55);
    discr::BlockData<REAL4> concentration(&block, 11.0);
    block.addVoxels(1, 1.0);

    discr::Block newBlock(raster, 2.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 1.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : block contains one cell whose top is positioned just above
  //         the half of the height of newBlock. newBlock also contains an
  //         equally sized cell.
  //
  //                      +-----+
  //         +-----+      |     |
  //         |     |  ->  |     |
  //         |     |      |     |
  //         |     |      +-----+
  //         +-----+
  //
  // Result: block ends up with the same value as before.
  {
    discr::Block block(raster, 1.6);
    discr::BlockData<UINT1> sand(&block, 1);
    discr::BlockData<INT4> sediment(&block, 66);
    discr::BlockData<REAL4> concentration(&block, 12.0);
    block.addVoxels(1, 1.0);

    discr::Block newBlock(raster, 2.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 1.0);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(1)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(66)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(12.0)));
  }

  //----------------------------------------------------------------------------
  // Test  : block contains two cells, one of which is thicker than the
  //         other and contains a missing value.
  //         newBlock contains one cell at the same height as block and
  //         with the same size as the two cells of block.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |-----|  ->  |     |
  //         +-----+      +-----+
  //
  // Result: block will contain a missing value.
  {
    discr::Block block(raster, 3.0);
    discr::BlockData<UINT1> sand(&block, 0);
    discr::BlockData<INT4> sediment(&block, 5);
    discr::BlockData<REAL4> concentration(&block, 3.0);
    block.addVoxels(1, 1.0);
    sand.setDefaultValueMissing();
    sediment.setDefaultValueMissing();
    concentration.setDefaultValueMissing();
    block.addVoxels(1, 1.1);

    discr::Block newBlock(raster, 3.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.1);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(pcr::isMV(newSand.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newSediment.cell(0)[0]));
    BOOST_CHECK(pcr::isMV(newConcentration.cell(0)[0]));
  }

  //----------------------------------------------------------------------------
  // Test  : block contains two cells, one of which is thinner than the
  //         other and contains a missing value.
  //         newBlock contains one cell at the same height as block and
  //         with the same size as the two cells of block.
  //
  //         +-----+      +-----+
  //         |     |      |     |
  //         |-----|  ->  |     |
  //         +-----+      +-----+
  //
  // Result: block will contain the non-missing value.
  {
    discr::Block block(raster, 3.0);
    discr::BlockData<UINT1> sand(&block, 1);
    discr::BlockData<INT4> sediment(&block, 88);
    discr::BlockData<REAL4> concentration(&block, 4.0);
    block.addVoxels(1, 1.1);
    sand.setDefaultValueMissing();
    sediment.setDefaultValueMissing();
    concentration.setDefaultValueMissing();
    block.addVoxels(1, 1.0);

    discr::Block newBlock(raster, 3.0);
    discr::BlockData<UINT1> newSand(&newBlock);
    discr::BlockData<INT4> newSediment(&newBlock);
    discr::BlockData<REAL4> newConcentration(&newBlock);
    newBlock.addVoxels(1, 2.1);

    resample<UINT1>(newSand, sand);
    resample<INT4>(newSediment, sediment);
    resample<REAL4>(newConcentration, concentration);

    BOOST_CHECK(dal::comparable(newSand.cell(0)[0], UINT1(1)));
    BOOST_CHECK(dal::comparable(newSediment.cell(0)[0], INT4(88)));
    BOOST_CHECK(dal::comparable(newConcentration.cell(0)[0], REAL4(4.0)));
  }

  //////////////////////////////////////////////////////////////////////////////
  // MISC TESTS WITH REAL LIFE SITUATIONS
  //////////////////////////////////////////////////////////////////////////////

  // //----------------------------------------------------------------------------
  // // Test  : block contains tree cells.
  // //         newBlock contains eleven cells.
  // // Result: block will contain 11 cells.
  // block.removeVoxels();
  // block.setBase(-9.99);
  // block.setDefaultValue(scalarAttr, 1.0);
  // block.addVoxel(5.02497);
  // block.addVoxel(5.02497);
  // block.addVoxel(0.199799);
  // // block.setDefaultValue(scalarAttr, mv);

  // newBlock.removeVoxels();
  // newBlock.setBase(-10.0);
  // newBlock.addVoxels(11, 1.0);

  // block.resample(newBlock.space());

  // BOOST_CHECK(block.space() == newBlock.space());

  // for(ScalarAttribute::const_iterator it = scalarAttr->begin();
  //                  it != scalarAttr->end(); ++it) {
  //   PRECOND((*it).size() == 1);
  //   BOOST_CHECK((*it)[0] == 4.0);
  // }
}
