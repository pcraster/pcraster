#define BOOST_TEST_MODULE pcrblock voxel_at_height
#include <boost/test/unit_test.hpp>
#include <algorithm>
#include "block_voxelatheight.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace block;

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
