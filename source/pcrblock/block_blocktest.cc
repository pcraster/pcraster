#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_BLOCKTEST
#include "block_blocktest.h"
#define INCLUDED_BLOCK_BLOCKTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SCOPED_PTR
#include <boost/scoped_ptr.hpp>
#define INCLUDED_BOOST_SCOPED_PTR
#endif

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
#ifndef INCLUDED_BLOCK_FUNCTIONS
#include "block_functions.h"
#define INCLUDED_BLOCK_FUNCTIONS
#endif



/*!
  \file
  This file contains the implementation of the BlockTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCK MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*BlockTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BlockTest> instance(new BlockTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testCreate, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testBaseElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testSurfaceElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BlockTest::testSetDefaultValue, instance));

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



void BlockTest::testCreate()
{
  discr::Raster raster(3, 1, 1.0, 0.0, 0.0);
  discr::RasterData<REAL4> elevation(&raster);
  elevation.cell(0) = REAL4(1.0);
  pcr::setMV(elevation.cell(1));
  elevation.cell(2) = REAL4(3.0);

  boost::scoped_ptr<discr::Block> block(create(&elevation));
  BOOST_CHECK(block->cell(0).empty());
  BOOST_CHECK(dal::comparable(block->cell(0).baseElevation(), elevation.cell(0)));
  BOOST_CHECK(block->cell(1).empty());
  BOOST_CHECK(block->cell(1).isMV());
  BOOST_CHECK(block->cell(2).empty());
  BOOST_CHECK(dal::comparable(block->cell(2).baseElevation(), elevation.cell(2)));
}



void BlockTest::testBaseElevation()
{
  typedef discr::Block::ThicknessType T;
  discr::Raster raster(3, 1);

  {
    discr::RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));
    elevation.cell(2) = 2.2;
    discr::Block block(elevation);

    discr::RasterData<REAL4> result(&raster);
    baseElevation(result, block);


    BOOST_CHECK(!pcr::isMV(result.cell(0)));
    BOOST_CHECK(dal::comparable(result.cell(0), T(3.0)));
    BOOST_CHECK( pcr::isMV(result.cell(1)));
    BOOST_CHECK(!pcr::isMV(result.cell(2)));
    BOOST_CHECK(dal::comparable(result.cell(2), T(2.2)));
  }
}



void BlockTest::testSurfaceElevation()
{
  typedef discr::Block::ThicknessType T;
  discr::Raster raster(3, 1);

  {
    discr::RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));
    elevation.cell(2) = 2.2;
    discr::Block block(elevation);

    discr::RasterData<REAL4> result(&raster);
    surfaceElevation(result, block);

    BOOST_CHECK(!pcr::isMV(result.cell(0)));
    BOOST_CHECK(dal::comparable(result.cell(0), T(3.0)));
    BOOST_CHECK( pcr::isMV(result.cell(1)));
    BOOST_CHECK(!pcr::isMV(result.cell(2)));
    BOOST_CHECK(dal::comparable(result.cell(2), T(2.2)));

    block.addVoxels(2, 5.0);

    surfaceElevation(result, block);
    BOOST_CHECK(!pcr::isMV(result.cell(0)));
    BOOST_CHECK(dal::comparable(result.cell(0), T(13.0)));
    BOOST_CHECK( pcr::isMV(result.cell(1)));
    BOOST_CHECK(!pcr::isMV(result.cell(2)));
    BOOST_CHECK(dal::comparable(result.cell(2), T(12.2)));
  }
}



void BlockTest::testSetDefaultValue()
{
  typedef discr::Block::ThicknessType T;
  discr::Raster raster(3, 1);

  {
    discr::RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));
    elevation.cell(2) = 2.2;
    discr::Block block(elevation);

    discr::BlockData<INT4> sediment(&block);

    block.addVoxels(1, REAL4(2.3));
    BOOST_CHECK(!block.cell(0).isMV());
    BOOST_CHECK(sediment.cell(0).size() == 1);
    BOOST_CHECK(pcr::isMV(sediment.cell(0)[0]));

    BOOST_CHECK( block.cell(1).isMV());
    BOOST_CHECK(sediment.cell(1).empty());

    BOOST_CHECK(!block.cell(2).isMV());
    BOOST_CHECK(sediment.cell(2).size() == 1);
    BOOST_CHECK(pcr::isMV(sediment.cell(2)[0]));

    discr::RasterData<INT4> values(&raster);
    values.cell(0) = 5;
    values.cell(1) = 6;
    pcr::setMV(values.cell(2));

    setDefaultValue(sediment, values);

    block.addVoxels(1, REAL4(2.3));

    BOOST_CHECK(!block.cell(0).isMV());
    BOOST_CHECK(sediment.cell(0).size() == 2);
    BOOST_CHECK( pcr::isMV(sediment.cell(0)[0]));
    BOOST_CHECK(!pcr::isMV(sediment.cell(0)[1]));
    BOOST_CHECK(sediment.cell(0)[1] == 5);

    BOOST_CHECK( block.cell(1).isMV());
    BOOST_CHECK(sediment.cell(1).empty());

    BOOST_CHECK(!block.cell(2).isMV());
    BOOST_CHECK(sediment.cell(2).size() == 2);
    BOOST_CHECK( pcr::isMV(sediment.cell(2)[0]));
    BOOST_CHECK( pcr::isMV(sediment.cell(2)[1]));
  }
}



} // namespace block

