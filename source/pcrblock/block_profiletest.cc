#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_PROFILETEST
#include "block_profiletest.h"
#define INCLUDED_BLOCK_PROFILETEST
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

// Module headers.
#ifndef INCLUDED_BLOCK_FUNCTIONS
#include "block_functions.h"
#define INCLUDED_BLOCK_FUNCTIONS
#endif



/*!
  \file
  This file contains the implementation of the ProfileTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROFILE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*ProfileTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ProfileTest> instance(new ProfileTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ProfileTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PROFILE MEMBERS
//------------------------------------------------------------------------------

//! ctor
ProfileTest::ProfileTest(
         )
{
}



//! setUp
void ProfileTest::setUp()
{
}



//! tearDown
void ProfileTest::tearDown()
{
}



void ProfileTest::test()
{
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

} // namespace block

