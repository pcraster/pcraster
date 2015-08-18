#ifndef INCLUDED_DAL_SPATIALCOORDINATETEST
#include "dal_SpatialCoordinateTest.h"
#define INCLUDED_DAL_SPATIALCOORDINATETEST
#endif

// External headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_FLOATING_POINT_COMPARISON
#include <boost/test/floating_point_comparison.hpp>
#define INCLUDED_BOOST_TEST_FLOATING_POINT_COMPARISON
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
#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif



/*!
  \file
  This file contains the implementation of the SpatialCoordinateTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPATIALCOORDINATETEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* SpatialCoordinateTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpatialCoordinateTest> instance(
         new SpatialCoordinateTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &SpatialCoordinateTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &SpatialCoordinateTest::testCopy, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &SpatialCoordinateTest::testAssignment, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &SpatialCoordinateTest::testEquals, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPATIALCOORDINATETEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
SpatialCoordinateTest::SpatialCoordinateTest()
{
}



void SpatialCoordinateTest::test()
{
  {
    SpatialCoordinate coordinate;
    BOOST_CHECK_CLOSE(coordinate.x(), 0.0, 0.001);
    BOOST_CHECK_CLOSE(coordinate.y(), 0.0, 0.001);
  }

  {
    SpatialCoordinate coordinate(5.5, 6.6);
    BOOST_CHECK_CLOSE(coordinate.x(), 5.5, 0.001);
    BOOST_CHECK_CLOSE(coordinate.y(), 6.6, 0.001);
  }
}



void SpatialCoordinateTest::testCopy()
{
  {
    SpatialCoordinate coordinate1;
    SpatialCoordinate coordinate2(coordinate1);

    BOOST_CHECK_CLOSE(coordinate2.x(), 0.0, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 0.0, 0.001);
  }

  {
    SpatialCoordinate coordinate1(5.5, 6.6);
    SpatialCoordinate coordinate2(coordinate1);

    BOOST_CHECK_CLOSE(coordinate2.x(), 5.5, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 6.6, 0.001);
  }
}



void SpatialCoordinateTest::testAssignment()
{
  {
    SpatialCoordinate coordinate1;
    SpatialCoordinate coordinate2 = coordinate1;

    BOOST_CHECK_CLOSE(coordinate2.x(), 0.0, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 0.0, 0.001);
  }

  {
    SpatialCoordinate coordinate1(5.5, 6.6);
    SpatialCoordinate coordinate2 = coordinate1;

    BOOST_CHECK_CLOSE(coordinate2.x(), 5.5, 0.001);
    BOOST_CHECK_CLOSE(coordinate2.y(), 6.6, 0.001);
  }
}



void SpatialCoordinateTest::testEquals()
{
  SpatialCoordinate coordinate1;
  SpatialCoordinate coordinate2;
  SpatialCoordinate coordinate3(5.5, 6.6);
  SpatialCoordinate coordinate4(5.5, 6.6);

  BOOST_CHECK(coordinate1 == coordinate2);
  BOOST_CHECK(coordinate1 != coordinate3);
  BOOST_CHECK(coordinate3 == coordinate4);
}

} // namespace dal

