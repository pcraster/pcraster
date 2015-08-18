#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_VOXELSTACK_VOXELSTACKTEST
#include "voxelstack_voxelstacktest.h"
#define INCLUDED_VOXELSTACK_VOXELSTACKTEST
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

// Module headers.



/*!
  \file
  This file contains the implementation of the VoxelStackTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace voxelstack {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VOXELSTACKTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*VoxelStackTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VoxelStackTest> instance(new VoxelStackTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VoxelStackTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VOXELSTACKTEST MEMBERS
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



void VoxelStackTest::test()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace voxelstack

