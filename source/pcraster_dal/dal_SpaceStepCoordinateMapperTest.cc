#ifndef INCLUDED_DAL_SPACESTEPCOORDINATEMAPPERTEST
#include "dal_SpaceStepCoordinateMapperTest.h"
#define INCLUDED_DAL_SPACESTEPCOORDINATEMAPPERTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_SPACESTEPCOORDINATEMAPPER
#include "dal_SpaceStepCoordinateMapper.h"
#define INCLUDED_DAL_SPACESTEPCOORDINATEMAPPER
#endif



/*!
  \file
  This file contains the implementation of the SpaceStepCoordinateMapperTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPACESTEPCOORDINATEMAPPERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* SpaceStepCoordinateMapperTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpaceStepCoordinateMapperTest> instance(
         new SpaceStepCoordinateMapperTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &SpaceStepCoordinateMapperTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPACESTEPCOORDINATEMAPPERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
SpaceStepCoordinateMapperTest::SpaceStepCoordinateMapperTest()
{
}



void SpaceStepCoordinateMapperTest::test()
{
  {
    SpaceStepCoordinateMapper mapper(1, 74.85, -0.30);

    std::vector<size_t> rows;
    rows.push_back(1);
    rows.push_back(250);
    rows.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Space, rows));
    DataSpaceAddress address = space.address();

    address.setCoordinate<size_t>(0, 1);
    BOOST_CHECK_EQUAL(mapper.toString(space, address, 0),  "74.85");
  }
}

} // namespace dal

