#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_VOXELATHEIGHTTEST
#include "block_voxelatheighttest.h"
#define INCLUDED_BLOCK_VOXELATHEIGHTTEST
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
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_BLOCK_VOXELATHEIGHT
#include "block_voxelatheight.h"
#define INCLUDED_BLOCK_VOXELATHEIGHT
#endif



/*!
  \file
  This file contains the implementation of the VoxelAtHeightTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VOXELATHEIGHT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*VoxelAtHeightTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VoxelAtHeightTest> instance(new VoxelAtHeightTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VoxelAtHeightTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VOXELATHEIGHT MEMBERS
//------------------------------------------------------------------------------

//! ctor
VoxelAtHeightTest::VoxelAtHeightTest(
         )
{
}



//! setUp
void VoxelAtHeightTest::setUp()
{
}



//! tearDown
void VoxelAtHeightTest::tearDown()
{
}



void VoxelAtHeightTest::test()
{
  {
    // Empty stack does not return true.
    VoxelAtHeight f(5.0, 5.0);
    BOOST_CHECK(!(f(0.0)));
    BOOST_CHECK( (f(1.0)));
  }

  {
    VoxelAtHeight f(5.0, 6.0);
    BOOST_CHECK(!(f(0.0)));
    BOOST_CHECK(!(f(0.5)));
    BOOST_CHECK(!(f(0.5)));
    BOOST_CHECK( (f(0.001)));
  }

  {
    VoxelAtHeight f(5.0, 9.0);
    BOOST_CHECK(!(f(0.0)));
    BOOST_CHECK(!(f(1.0)));
    BOOST_CHECK(!(f(1.0)));
    BOOST_CHECK(!(f(2.0)));
    BOOST_CHECK( (f(2.0)));
  }

  {
    VoxelAtHeight f(0.0, 4.0);
    BOOST_CHECK(!f(0.0));
    BOOST_CHECK(!f(1.0));
    BOOST_CHECK(!f(1.0));
    BOOST_CHECK(!f(1.0));

    // Top of the voxel does not return true here.
    BOOST_CHECK(!f(1.0));
    BOOST_CHECK(f(0.001));
  }

  {
    VoxelAtHeight f(-1.0, 1.0);
    BOOST_CHECK(!f(0.0));
    BOOST_CHECK(!f(1.0));
    BOOST_CHECK(!f(1.0));
    BOOST_CHECK(f(0.001));
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.0);
    thicknesses.push_back(1.0);
    thicknesses.push_back(1.0);
    thicknesses.push_back(1.0);
    thicknesses.push_back(1.0);

    std::vector<REAL4>::const_iterator it;

    // Bottom of the first voxel equals the requested height and returns true.
    it = std::find_if(
           thicknesses.begin(), thicknesses.end(), VoxelAtHeight(0.0, 0.0));
    BOOST_CHECK(it == thicknesses.begin());

    // Bottom of the fourth voxel equals the requested height and returns true.
    it = std::find_if(
           thicknesses.begin(), thicknesses.end(), VoxelAtHeight(0.0, 3.0));
    BOOST_CHECK(it == thicknesses.begin() + 3);

    // Top of the voxel equals the requested heigth but does not return true.
    it = std::find_if(
           thicknesses.begin(), thicknesses.end(), VoxelAtHeight(0.0, 5.0));
    BOOST_CHECK(it == thicknesses.end());

    it = std::find_if(
           thicknesses.begin(), thicknesses.end(), VoxelAtHeight(0.0, 6.0));
    BOOST_CHECK(it == thicknesses.end());
  }
}

} // namespace block

