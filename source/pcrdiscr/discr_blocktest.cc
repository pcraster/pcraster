#define BOOST_TEST_MODULE pcraster discr block
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "discr_block.h"
#include "discr_raster.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace discr;

  size_t nrRows = 3;
  size_t nrCols = 4;
  double cellSize = 1.5;
  double west = 1.0;
  double north = 0.0;

  Raster raster(nrRows, nrCols, cellSize, west, north);

  {
    Block block(nrRows, nrCols, cellSize, west, north);
    BOOST_CHECK(block.nrRows() == nrRows);
    BOOST_CHECK(block.nrCols() == nrCols);
    BOOST_CHECK(block.cellSize() == cellSize);
    BOOST_CHECK(block.west() == west);
    BOOST_CHECK(block.north() == north);
    BOOST_CHECK(block.nrCells() == nrRows * nrCols);

    for(size_t i = 0; i < block.nrCells(); ++i) {
      BOOST_CHECK(!block.cell(i).isMV());
    }
  }

  {
    Block block(raster);
    BOOST_CHECK(block.nrRows() == nrRows);
    BOOST_CHECK(block.nrCols() == nrCols);
    BOOST_CHECK(block.cellSize() == cellSize);
    BOOST_CHECK(block.west() == west);
    BOOST_CHECK(block.north() == north);
    BOOST_CHECK(block.nrCells() == nrRows * nrCols);

    for(size_t i = 0; i < block.nrCells(); ++i) {
      BOOST_CHECK(!block.cell(i).isMV());
    }
  }

  {
    Block block(raster, 5.0);
    BOOST_CHECK(block.nrRows() == nrRows);
    BOOST_CHECK(block.nrCols() == nrCols);
    BOOST_CHECK(block.cellSize() == cellSize);
    BOOST_CHECK(block.west() == west);
    BOOST_CHECK(block.north() == north);
    BOOST_CHECK(block.nrCells() == nrRows * nrCols);

    for(size_t i = 0; i < block.nrCells(); ++i) {
      BOOST_CHECK(!block.cell(i).isMV());
      BOOST_CHECK(dal::comparable(block.cell(i).baseElevation(),
                Block::ThicknessType(5.0)));
    }
  }

  typedef Block::ThicknessType T;

  {
    RasterData<REAL4> baseElevation(&raster, 3.0);
    baseElevation.cell(0) = 1.1F;
    baseElevation.cell(5) = 2.2F;
    baseElevation.cell(9) = 3.3F;
    baseElevation.cell(11) = 4.4F;
    Block block(baseElevation);
    BOOST_CHECK(block.nrRows() == nrRows);
    BOOST_CHECK(block.nrCols() == nrCols);
    BOOST_CHECK(block.cellSize() == cellSize);
    BOOST_CHECK(block.west() == west);
    BOOST_CHECK(block.north() == north);
    BOOST_CHECK(block.nrCells() == nrRows * nrCols);

    for(size_t i = 0; i < block.nrCells(); ++i) {
      BOOST_CHECK(!block.cell(i).isMV());
    }

    BOOST_CHECK(dal::comparable(block.cell(0).baseElevation(), T(1.1)));
    BOOST_CHECK(dal::comparable(block.cell(1).baseElevation(), T(3.0)));
    BOOST_CHECK(dal::comparable(block.cell(5).baseElevation(), T(2.2)));
    BOOST_CHECK(dal::comparable(block.cell(10).baseElevation(), T(3.0)));
    BOOST_CHECK(dal::comparable(block.cell(11).baseElevation(), T(4.4)));
  }
}


BOOST_AUTO_TEST_CASE(add_voxels)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1);
  typedef Block::ThicknessType T;

  {
    Block block(raster, 5.0);
    block.addVoxels(3, 1.2F);
    BOOST_CHECK(block.cell(0).size() == 3);
    BOOST_CHECK(block.cell(1).size() == 3);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(3.6)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(3.6)));

    block.addVoxel(0, 1.4F);
    BOOST_CHECK(block.cell(0).size() == 4);
    BOOST_CHECK(block.cell(1).size() == 3);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(5.0)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(3.6)));

    block.addVoxels(1, 2, 1.4F);
    BOOST_CHECK(block.cell(0).size() == 4);
    BOOST_CHECK(block.cell(1).size() == 5);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(5.0)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(6.4)));
  }

  {
    RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));

    Block block(elevation);
    block.addVoxels(3, 1.2F);
    BOOST_CHECK(!block.cell(0).isMV());
    BOOST_CHECK( block.cell(0).size() == 3);

    BOOST_CHECK( block.cell(1).isMV());
    BOOST_CHECK( block.cell(1).size() == 0);

    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(3.6)));
  }
}


BOOST_AUTO_TEST_CASE(nr_voxels)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);

  {
    Block block(raster, 5.0);
    BOOST_CHECK(block.nrVoxels() == 0);

    block.addVoxels(3, 1.2F);
    BOOST_CHECK(block.nrVoxels() == 6);

    block.addVoxels(3, 1.2F);
    BOOST_CHECK(block.nrVoxels() == 12);

    block.removeVoxels(0, 2);
    BOOST_CHECK(block.nrVoxels() == 10);

    block.setMV(1);
    BOOST_CHECK(block.nrVoxels() == 4);
  }
}


BOOST_AUTO_TEST_CASE(remove_voxels)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

  {
    Block block(raster, 5.0);
    block.addVoxels(3, 1.2F);
    block.removeVoxels(0, 1);
    BOOST_CHECK(block.cell(0).size() == 2);
    BOOST_CHECK(block.cell(1).size() == 3);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(2.4)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(3.6)));

    block.removeVoxels(1, 2);
    BOOST_CHECK(block.cell(0).size() == 2);
    BOOST_CHECK(block.cell(1).size() == 1);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(2.4)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(1.2)));

    block.removeVoxels(0, 2);
    BOOST_CHECK(block.cell(0).size() == 0);
    BOOST_CHECK(block.cell(1).size() == 1);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(0.0)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(1.2)));

    block.removeVoxels(1, 1);
    BOOST_CHECK(block.cell(0).size() == 0);
    BOOST_CHECK(block.cell(1).size() == 0);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(0.0)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(0.0)));
  }
}


BOOST_AUTO_TEST_CASE(cut_voxels)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

  {
    Block block(raster, 5.0);
    block.addVoxels(3, 1.2F);
    block.cutVoxel(0, 0.5);
    BOOST_CHECK(block.cell(0).size() == 3);
    BOOST_CHECK(block.cell(1).size() == 3);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(3.0)));
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(3.6)));
  }
}


BOOST_AUTO_TEST_CASE(set_mv)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);

  {
    Block block(raster, 5.0);
    block.addVoxels(0, 3, 1.2F);
    BOOST_CHECK(!block.cell(0).isMV());
    BOOST_CHECK(!block.cell(1).isMV());
    BOOST_CHECK(block.cell(0).size() == 3);
    BOOST_CHECK(block.cell(1).size() == 0);

    block.setMV(0);
    BOOST_CHECK( block.cell(0).isMV());
    BOOST_CHECK(!block.cell(1).isMV());
    BOOST_CHECK(block.cell(0).size() == 0);
    BOOST_CHECK(block.cell(1).size() == 0);

    block.setMV(1);
    BOOST_CHECK( block.cell(0).isMV());
    BOOST_CHECK( block.cell(1).isMV());
    BOOST_CHECK(block.cell(0).size() == 0);
    BOOST_CHECK(block.cell(1).size() == 0);
  }
}


BOOST_AUTO_TEST_CASE(equals)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);

  {
    Block block1(raster, 5.0);
    Block block2(raster, 6.0);
    BOOST_CHECK(block1 == block1);
    BOOST_CHECK(block2 == block2);
    BOOST_CHECK(block1 != block2);
    BOOST_CHECK(block2 != block1);

    Block block3(raster);
    BOOST_CHECK(block3 == block3);
    BOOST_CHECK(block3 != block1);

    block3.cell(0).setBaseElevation(5.0);
    block3.cell(1).setBaseElevation(5.0);
    BOOST_CHECK(block3 == block1);

    block1.addVoxels(2, 3.0);
    BOOST_CHECK(block1 != block3);

    block3.addVoxels(2, 3.0);
    BOOST_CHECK(block1 == block3);
  }
}


BOOST_AUTO_TEST_CASE(is_empty)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);

  {
    Block block(raster, 5.0);
    BOOST_CHECK(block.isEmpty());

    block.addVoxels(1, 1.0);
    BOOST_CHECK(!block.isEmpty());

    block.removeVoxels(0, 1);
    block.removeVoxels(1, 1);
    BOOST_CHECK(block.isEmpty());

    block.addVoxels(1, 1.0);
    BOOST_CHECK(!block.isEmpty());
    block.setMV(0);
    block.setMV(1);
    BOOST_CHECK(block.isEmpty());
  }
}


BOOST_AUTO_TEST_CASE(is_regular)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);

  {
    // empty block
    Block block(raster, 5.0);
    BOOST_CHECK(block.isRegular());

    block.addVoxels(3, 1.2F);
    BOOST_CHECK(block.isRegular());

    block.cell(1)[1] = 1.1F;
    BOOST_CHECK(!block.isRegular());

    block.cell(1)[1] = 1.2F;
    BOOST_CHECK(block.isRegular());

    // some cells mv
    block.setMV(0);
    BOOST_CHECK(!block.isRegular());

    // only mv's
    block.setMV(1);
    BOOST_CHECK(block.isRegular());
  }
}


BOOST_AUTO_TEST_CASE(bottom_elevation)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  bool isValid;
  typedef Block::ThicknessType T;
  T elevation;

  {
    Block block(raster);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T()));

    block.setMV(0);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T()));

    block.setMV(1);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(!isValid);
  }

  {
    Block block(raster, 5.0);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(5.0)));

    block.cell(1).setBaseElevation(6.0);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(5.0)));
  }

  {
    Block block(raster, 5.0);
    block.addVoxels(3, 1.2F);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(5.0)));

    block.cell(1).setBaseElevation(-10.0);
    isValid = block.bottomElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(-10.0)));
  }
}


BOOST_AUTO_TEST_CASE(top_elevation)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  bool isValid;
  typedef Block::ThicknessType T;
  T elevation;

  {
    Block block(raster);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T()));

    block.setMV(0);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T()));

    block.setMV(1);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(!isValid);
  }

  {
    Block block(raster, 5.0);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(5.0)));

    block.cell(1).setBaseElevation(6.0);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(6.0)));
  }

  {
    Block block(raster, 5.0);
    block.addVoxels(3, 1.2F);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(8.6)));

    block.cell(1).setBaseElevation(10.0);
    isValid = block.topElevation(elevation);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(elevation, T(13.6)));
  }
}


BOOST_AUTO_TEST_CASE(extreme_elevations)
{
  using namespace discr;

  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  bool isValid;
  typedef Block::ThicknessType T;
  T bottom, top;

  {
    Block block(raster);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(top, T()));
    BOOST_CHECK(dal::comparable(bottom, T()));

    block.setMV(0);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(top, T()));
    BOOST_CHECK(dal::comparable(bottom, T()));

    block.setMV(1);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(!isValid);
  }

  {
    Block block(raster, 5.0);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(top, T(5.0)));
    BOOST_CHECK(dal::comparable(bottom, T(5.0)));

    block.cell(1).setBaseElevation(6.0);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(top, T(6.0)));
    BOOST_CHECK(dal::comparable(bottom, T(5.0)));
  }

  {
    Block block(raster, 5.0);
    block.addVoxels(3, 1.2F);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(top, T(8.6)));
    BOOST_CHECK(dal::comparable(bottom, T(5.0)));

    block.cell(1).setBaseElevation(10.0);
    isValid = block.extremeElevations(bottom, top);
    BOOST_CHECK(isValid);
    BOOST_CHECK(dal::comparable(top, T(13.6)));
    BOOST_CHECK(dal::comparable(bottom, T(5.0)));
  }
}
