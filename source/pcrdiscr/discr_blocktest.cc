#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_BLOCKTEST
#include "discr_blocktest.h"
#define INCLUDED_DISCR_BLOCKTEST
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

// Module headers.
#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif



/*!
  \file
  This file contains the implementation of the BlockTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCK MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*BlockTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BlockTest> instance(new BlockTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testAddVoxels, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testNrVoxels, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testRemoveVoxels, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testCutVoxels, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testSetMV, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testEquals, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testIsEmpty, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testIsRegular, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testBottomELevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testTopElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testExtremeElevations, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BLOCK MEMBERS
//------------------------------------------------------------------------------

//! ctor
BlockTest::BlockTest(
         )
{
}



//! setUp
void BlockTest::setUp()
{
}



//! tearDown
void BlockTest::tearDown()
{
}



void BlockTest::testConstructor()
{
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



void BlockTest::testAddVoxels()
{
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



void BlockTest::testNrVoxels()
{
  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

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



void BlockTest::testRemoveVoxels()
{
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



void BlockTest::testCutVoxels()
{
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



void BlockTest::testSetMV()
{
  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

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



void BlockTest::testEquals()
{
  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

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



void BlockTest::testIsEmpty()
{
  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

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



void BlockTest::testIsRegular()
{
  // Two rows, one column.
  Raster raster(2, 1, 1.0, 0.0, 0.0);
  typedef Block::ThicknessType T;

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



void BlockTest::testBottomELevation()
{
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



void BlockTest::testTopElevation()
{
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



void BlockTest::testExtremeElevations()
{
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

} // namespace discr

