#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_ADDTEST
#include "block_addtest.h"
#define INCLUDED_BLOCK_ADDTEST
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
  This file contains the implementation of the AddTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ADD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*AddTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AddTest> instance(new AddTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AddTest::testNoCompactionAdd, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&AddTest::testMackyBridgeAdd, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&AddTest::testDeHaanAdd, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ADD MEMBERS
//------------------------------------------------------------------------------

//! ctor
AddTest::AddTest(
         )
{
}



//! setUp
void AddTest::setUp()
{
}



//! tearDown
void AddTest::tearDown()
{
}



void AddTest::testNoCompactionAdd()
{
  {
    // Create block.
    // Create raster with thicknesses.
    // Add voxels.
    // Test block.

    discr::Raster raster(3, 1, 1.0, 0.0, 0.0);
    discr::RasterData<REAL4> baseElevation(&raster, REAL4(-5.0));
    discr::Block block(baseElevation);
    typedef discr::Block::ThicknessType T;

    discr::RasterData<T> thicknesses(&raster);
    thicknesses.cell(0) = T(0.0);
    thicknesses.cell(1) = T(1.0);
    pcr::setMV(thicknesses.cell(2));

    noCompactionAdd(block, thicknesses);

    BOOST_CHECK(dal::comparable(block.cell(0).baseElevation(), T(-5.0)));
    BOOST_CHECK(block.cell(0).size() == 0);
    BOOST_CHECK(dal::comparable(block.cell(0).thickness(), T(0.0)));
    BOOST_CHECK(dal::comparable(block.cell(0).surfaceElevation(), T(-5.0)));

    BOOST_CHECK(dal::comparable(block.cell(1).baseElevation(), T(-5.0)));
    BOOST_CHECK(block.cell(1).size() == 1);
    BOOST_CHECK(dal::comparable(block.cell(1).thickness(), T(1.0)));
    BOOST_CHECK(dal::comparable(block.cell(1).surfaceElevation(), T(-4.0)));

    BOOST_CHECK(block.cell(2).isMV());
  }
}



void AddTest::testMackyBridgeAdd()
{
  size_t nrRows = 3;
  size_t nrCols = 4;
  double cellSize = 1.5;
  double west = 1.0;
  double north = 0.0;

  // Discretisations.
  discr::Raster raster(nrRows, nrCols, cellSize, west, north);
  discr::RasterData<REAL4> baseElevation(&raster, REAL4(5.0));
  discr::Block block(baseElevation);

  // Data.
  discr::BlockData<REAL4> originalThickness(&block);
  discr::BlockData<INT4> sediment(&block, 3);

  // Add function arguments.
  discr::RasterData<REAL4> thickness(&raster, REAL4(15.5));
  REAL4 maxVoxelThickness = REAL4(1.0);

  // No compaction.
  Compactors<MackeyBridgeCompactor> compactors;
  compactors.setCompactor(3, DummyCompactor());

  // TODO work around this add, see ResampleTest, remove functions.h inclusion.
  mackeyBridgeAdd(block, originalThickness, sediment, thickness,
         maxVoxelThickness, compactors);

  // Check discretisation.
  for(size_t i = 0; i < block.nrCells(); ++i) {
    BOOST_CHECK(block.cell(i).size() == 16);
    BOOST_CHECK(block.cell(i).thickness() == REAL4(15.5));
    BOOST_CHECK(block.cell(i).back() == REAL4(0.5));
  }

  // Check data.
  for(size_t i = 0; i < block.nrCells(); ++i) {
    for(size_t j = 0; j < block.cell(i).size() - 1; ++j) {
      BOOST_CHECK(originalThickness.cell(i)[j] == maxVoxelThickness);
      BOOST_CHECK(sediment.cell(i)[j] == 3);
    }

    BOOST_CHECK(originalThickness.cell(i).back() == REAL4(0.5));
    BOOST_CHECK(sediment.cell(i).back() == 3);
  }
}



void AddTest::testDeHaanAdd()
{
  size_t nrRows = 1;
  size_t nrCols = 1;
  double cellSize = 1.0;
  double west = 0.0;
  double north = 0.0;

  REAL4 timeStepDuration = 36000.0;
  size_t nrTimeSteps = 22;

  discr::Raster raster(nrRows, nrCols, cellSize, west, north);
  discr::RasterData<REAL4> baseElevation(&raster, 0.0);
  discr::Block block(baseElevation);

  discr::BlockData<REAL4> initialThickness(&block);
  discr::BlockData<REAL4> cummulativeLoad(&block);
  discr::BlockData<REAL4> cummulativeDuration(&block, timeStepDuration);
  discr::BlockData<INT4> sediment(&block, 3);

  discr::RasterData<REAL4> thickness(&raster, 2.0);

  Compactors<DeHaanCompactor> compactors;
  compactors.setCompactor(3, DeHaanCompactor(0.2, 0.02, 6.0));

  for(size_t i = 1; i <= nrTimeSteps; ++i) {
    deHaanAdd(block, sediment, initialThickness, cummulativeLoad,
         cummulativeDuration, thickness, compactors);
  }

  // Check discretisation.
  PRECOND(block.nrCells() == 1);
  discr::VoxelStack const& stack(block.cell(0));
  BOOST_CHECK(!stack.isMV());
  BOOST_CHECK(stack.size() == nrTimeSteps);

  // From spreadsheet Theo:
  // 1.377602705
  // 1.465631195
  // 1.511233963
  // 1.541209414
  // 1.563188267
  // 1.580362374
  // 1.594355
  // 1.60609826
  // 1.61617404
  // 1.624968544
  // 1.632750422
  // 1.639713639
  // 1.646002529
  // 1.651727223
  // 1.656973542
  // 1.661809581
  // 1.666290221
  // 1.670460297
  // 1.674356882
  // 1.678010954
  // 1.681448649
  // 1.684692206

  BOOST_CHECK(dal::comparable(stack[nrTimeSteps - 1], REAL4(1.377602705)));
  BOOST_CHECK(dal::comparable(stack[0], REAL4(1.684692206)));
}

} // namespace block
