#define BOOST_TEST_MODULE pcraster discr voxel_stack
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "discr_voxelstack.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace discr;

  {
    VoxelStack const stack;
    BOOST_TEST(!stack.isMV());
    BOOST_TEST(stack.baseElevation() == VoxelStack::value_type(0.0));
    BOOST_TEST(stack.empty());
    BOOST_TEST(stack.size() == 0);
  }

  {
    VoxelStack const stack(5.0);
    BOOST_TEST(!stack.isMV());
    BOOST_TEST(stack.baseElevation() == VoxelStack::value_type(5.0));
    BOOST_TEST(stack.empty());
    BOOST_TEST(stack.size() == 0);
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.1F);
    thicknesses.push_back(2.2F);
    thicknesses.push_back(0.0F);
    thicknesses.push_back(3.3F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_TEST(!stack.isMV());
    BOOST_TEST(stack.baseElevation() == VoxelStack::value_type(0.0));
    BOOST_TEST(stack.size() == thicknesses.size());
    BOOST_TEST(dal::comparable(stack[0], VoxelStack::value_type(1.1)));
    BOOST_TEST(dal::comparable(stack[1], VoxelStack::value_type(2.2)));
    BOOST_TEST(dal::comparable(stack[3], VoxelStack::value_type(3.3)));
  }
}


BOOST_AUTO_TEST_CASE(set_mv)
{
  using namespace discr;

  {
    VoxelStack stack;
    BOOST_TEST(!stack.isMV());
    stack.setMV();
    BOOST_TEST(stack.isMV());
  }
}


BOOST_AUTO_TEST_CASE(set_base_elevation)
{
  using namespace discr;

  {
    VoxelStack stack;
    stack.setBaseElevation(3.0);
    BOOST_TEST(stack.baseElevation() == VoxelStack::value_type(3.0));
  }
}


BOOST_AUTO_TEST_CASE(thickness)
{
  using namespace discr;

  {
    VoxelStack stack;
    BOOST_TEST(stack.thickness() == VoxelStack::value_type(0.0));

    stack.push_back(1.0);
    stack.push_back(2.0);
    BOOST_TEST(stack.thickness() == VoxelStack::value_type(3.0));
  }
}


BOOST_AUTO_TEST_CASE(is_regular)
{
  using namespace discr;

  {
    VoxelStack const stack;
    BOOST_TEST(stack.isRegular());
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_TEST(!stack.isRegular());
    stack[2] = 1.2F;
    BOOST_TEST(stack.isRegular());
  }

}


BOOST_AUTO_TEST_CASE(surface_elevation)
{
  using namespace discr;

  {
    VoxelStack const stack;
    BOOST_TEST(dal::comparable(stack.surfaceElevation(),
         VoxelStack::value_type(0.0)));
  }

  {
    VoxelStack const stack(5.0);
    BOOST_TEST(dal::comparable(stack.surfaceElevation(),
         VoxelStack::value_type(5.0)));
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    VoxelStack const stack(thicknesses.begin(), thicknesses.end());
    BOOST_TEST(dal::comparable(stack.surfaceElevation(),
         VoxelStack::value_type(3.7)));
  }
}


BOOST_AUTO_TEST_CASE(bottom_elevation)
{
  using namespace discr;

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    thicknesses.push_back(1.4F);
    VoxelStack const stack(thicknesses.begin(), thicknesses.end());
    BOOST_TEST(dal::comparable(stack.bottomElevation(0),
         VoxelStack::value_type(0.0)));
    BOOST_TEST(dal::comparable(stack.bottomElevation(1),
         VoxelStack::value_type(1.2)));
    BOOST_TEST(dal::comparable(stack.bottomElevation(2),
         VoxelStack::value_type(2.5)));
  }
}


BOOST_AUTO_TEST_CASE(top_elevation)
{
  using namespace discr;

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    thicknesses.push_back(1.4F);
    VoxelStack const stack(thicknesses.begin(), thicknesses.end());
    BOOST_TEST(dal::comparable(stack.topElevation(0),
         VoxelStack::value_type(1.2)));
    BOOST_TEST(dal::comparable(stack.topElevation(1),
         VoxelStack::value_type(2.5)));
    BOOST_TEST(dal::comparable(stack.topElevation(2),
         VoxelStack::value_type(3.9)));
  }
}


BOOST_AUTO_TEST_CASE(equals)
{
  using namespace discr;

  {
    VoxelStack stack1;
    VoxelStack const stack2;
    BOOST_TEST(stack1 == stack1);
    BOOST_TEST(stack1 == stack2);
    BOOST_TEST(stack2 == stack1);

    stack1.setBaseElevation(1.0);
    BOOST_TEST(stack1 == stack1);
    BOOST_TEST(stack1 != stack2);
    BOOST_TEST(stack2 != stack1);
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    thicknesses.push_back(1.4F);
    VoxelStack stack1(thicknesses.begin(), thicknesses.end());
    VoxelStack stack2(thicknesses.begin(), thicknesses.end());

    BOOST_TEST(stack1 == stack1);
    BOOST_TEST(stack1 == stack2);
    BOOST_TEST(stack2 == stack1);

    stack1.setBaseElevation(5.0);
    BOOST_TEST(stack1 == stack1);
    BOOST_TEST(stack1 != stack2);
    BOOST_TEST(stack2 != stack1);

    stack2.setBaseElevation(5.0);
    BOOST_TEST(stack2 == stack2);
    BOOST_TEST(stack1 == stack2);
    BOOST_TEST(stack2 == stack1);

    stack1[1] = 3.1F;
    BOOST_TEST(stack1 == stack1);
    BOOST_TEST(stack1 != stack2);
    BOOST_TEST(stack2 != stack1);

    stack2[1] = 3.1F;
    BOOST_TEST(stack2 == stack2);
    BOOST_TEST(stack1 == stack2);
    BOOST_TEST(stack2 == stack1);
  }
}
