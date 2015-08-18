#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_VOXELSTACKTEST
#include "discr_voxelstacktest.h"
#define INCLUDED_DISCR_VOXELSTACKTEST
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
#ifndef INCLUDED_DISCR_VOXELSTACK
#include "discr_voxelstack.h"
#define INCLUDED_DISCR_VOXELSTACK
#endif



/*!
  \file
  This file contains the implementation of the VoxelStackTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VOXELSTACK MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*VoxelStackTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VoxelStackTest> instance(new VoxelStackTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testSetMV, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testSetBaseElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testThickness, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testIsRegular, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testSurfaceElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testBottomElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testTopElevation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::testEquals, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VOXELSTACK MEMBERS
//------------------------------------------------------------------------------

//! ctor
VoxelStackTest::VoxelStackTest(
         )
{
}



//! setUp
void VoxelStackTest::setUp()
{
}



//! tearDown
void VoxelStackTest::tearDown()
{
}



void VoxelStackTest::testConstructor()
{
  {
    VoxelStack stack;
    BOOST_CHECK(!stack.isMV());
    BOOST_CHECK(stack.baseElevation() == VoxelStack::value_type(0.0));
    BOOST_CHECK(stack.empty());
    BOOST_CHECK(stack.size() == 0);
  }

  {
    VoxelStack stack(5.0);
    BOOST_CHECK(!stack.isMV());
    BOOST_CHECK(stack.baseElevation() == VoxelStack::value_type(5.0));
    BOOST_CHECK(stack.empty());
    BOOST_CHECK(stack.size() == 0);
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.1F);
    thicknesses.push_back(2.2F);
    thicknesses.push_back(0.0F);
    thicknesses.push_back(3.3F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_CHECK(!stack.isMV());
    BOOST_CHECK(stack.baseElevation() == VoxelStack::value_type(0.0));
    BOOST_CHECK(stack.size() == thicknesses.size());
    BOOST_CHECK(dal::comparable(stack[0], VoxelStack::value_type(1.1)));
    BOOST_CHECK(dal::comparable(stack[1], VoxelStack::value_type(2.2)));
    BOOST_CHECK(dal::comparable(stack[3], VoxelStack::value_type(3.3)));
  }
}



void VoxelStackTest::testSetMV()
{
  {
    VoxelStack stack;
    BOOST_CHECK(!stack.isMV());
    stack.setMV();
    BOOST_CHECK(stack.isMV());
  }
}



void VoxelStackTest::testSetBaseElevation()
{
  {
    VoxelStack stack;
    stack.setBaseElevation(3.0);
    BOOST_CHECK(stack.baseElevation() == VoxelStack::value_type(3.0));
  }
}



void VoxelStackTest::testThickness()
{
  {
    VoxelStack stack;
    BOOST_CHECK(stack.thickness() == VoxelStack::value_type(0.0));

    stack.push_back(1.0);
    stack.push_back(2.0);
    BOOST_CHECK(stack.thickness() == VoxelStack::value_type(3.0));
  }
}



void VoxelStackTest::testIsRegular()
{
  {
    VoxelStack stack;
    BOOST_CHECK(stack.isRegular());
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_CHECK(!stack.isRegular());
    stack[2] = 1.2F;
    BOOST_CHECK(stack.isRegular());
  }

}



void VoxelStackTest::testSurfaceElevation()
{
  {
    VoxelStack stack;
    BOOST_CHECK(dal::comparable(stack.surfaceElevation(),
         VoxelStack::value_type(0.0)));
  }

  {
    VoxelStack stack(5.0);
    BOOST_CHECK(dal::comparable(stack.surfaceElevation(),
         VoxelStack::value_type(5.0)));
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_CHECK(dal::comparable(stack.surfaceElevation(),
         VoxelStack::value_type(3.7)));
  }
}



void VoxelStackTest::testBottomElevation()
{
  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    thicknesses.push_back(1.4F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_CHECK(dal::comparable(stack.bottomElevation(0),
         VoxelStack::value_type(0.0)));
    BOOST_CHECK(dal::comparable(stack.bottomElevation(1),
         VoxelStack::value_type(1.2)));
    BOOST_CHECK(dal::comparable(stack.bottomElevation(2),
         VoxelStack::value_type(2.5)));
  }
}



void VoxelStackTest::testTopElevation()
{
  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    thicknesses.push_back(1.4F);
    VoxelStack stack(thicknesses.begin(), thicknesses.end());
    BOOST_CHECK(dal::comparable(stack.topElevation(0),
         VoxelStack::value_type(1.2)));
    BOOST_CHECK(dal::comparable(stack.topElevation(1),
         VoxelStack::value_type(2.5)));
    BOOST_CHECK(dal::comparable(stack.topElevation(2),
         VoxelStack::value_type(3.9)));
  }
}



void VoxelStackTest::testEquals()
{
  {
    VoxelStack stack1;
    VoxelStack stack2;
    BOOST_CHECK(stack1 == stack1);
    BOOST_CHECK(stack1 == stack2);
    BOOST_CHECK(stack2 == stack1);

    stack1.setBaseElevation(1.0);
    BOOST_CHECK(stack1 == stack1);
    BOOST_CHECK(stack1 != stack2);
    BOOST_CHECK(stack2 != stack1);
  }

  {
    std::vector<REAL4> thicknesses;
    thicknesses.push_back(1.2F);
    thicknesses.push_back(1.3F);
    thicknesses.push_back(1.4F);
    VoxelStack stack1(thicknesses.begin(), thicknesses.end());
    VoxelStack stack2(thicknesses.begin(), thicknesses.end());

    BOOST_CHECK(stack1 == stack1);
    BOOST_CHECK(stack1 == stack2);
    BOOST_CHECK(stack2 == stack1);

    stack1.setBaseElevation(5.0);
    BOOST_CHECK(stack1 == stack1);
    BOOST_CHECK(stack1 != stack2);
    BOOST_CHECK(stack2 != stack1);

    stack2.setBaseElevation(5.0);
    BOOST_CHECK(stack2 == stack2);
    BOOST_CHECK(stack1 == stack2);
    BOOST_CHECK(stack2 == stack1);

    stack1[1] = 3.1F;
    BOOST_CHECK(stack1 == stack1);
    BOOST_CHECK(stack1 != stack2);
    BOOST_CHECK(stack2 != stack1);

    stack2[1] = 3.1F;
    BOOST_CHECK(stack2 == stack2);
    BOOST_CHECK(stack1 == stack2);
    BOOST_CHECK(stack2 == stack1);
  }
}

} // namespace discr

