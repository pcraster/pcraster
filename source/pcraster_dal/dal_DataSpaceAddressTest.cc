#ifndef INCLUDED_DAL_DATASPACEADDRESSTEST
#include "dal_DataSpaceAddressTest.h"
#define INCLUDED_DAL_DATASPACEADDRESSTEST
#endif

// Library headers.
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

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif



/*!
  \file
  This file contains the implementation of the DataSpaceAddressTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEADDRESS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::DataSpaceAddressTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DataSpaceAddressTest> instance(new DataSpaceAddressTest());

  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceAddressTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceAddressTest::testCopy, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEADDRESS MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::DataSpaceAddressTest::DataSpaceAddressTest()
{
}



void dal::DataSpaceAddressTest::test()
{
  {
    DataSpaceAddress address(1);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    address.setCoordinate<int>(0, 5);
    BOOST_CHECK_EQUAL(address.coordinate<int>(0), 5);

    // Change type in place.
    address.setCoordinate<double>(0, 5.5);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK_EQUAL(address.coordinate<double>(0), 5.5);

    // Add a type.
    address.addCoordinate<std::string>("vijf");
    BOOST_CHECK_EQUAL(address.size(), size_t(2));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(1), "vijf");
  }

  {
    // scenario / time / space / space
    DataSpaceAddress address(4);
    BOOST_CHECK_EQUAL(address.size(), size_t(4));
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 50);
    address.setCoordinate<size_t>(2, 4);
    address.setCoordinate<size_t>(3, 5);

    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(1), size_t(50));
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(2), size_t(4));
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(3), size_t(5));
  }
}



void dal::DataSpaceAddressTest::testCopy()
{
  DataSpaceAddress address1(1);
  address1.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(5.5, 6.6));

  DataSpaceAddress address2(address1);
  BOOST_CHECK_CLOSE(address2.coordinate<SpatialCoordinate>(0).x(), 5.5, 0.001);
  BOOST_CHECK_CLOSE(address2.coordinate<SpatialCoordinate>(0).y(), 6.6, 0.001);

  boost::any const& any = address2.coordinate(0);
  SpatialCoordinate const& coordinate(
         boost::any_cast<SpatialCoordinate const&>(any));

  BOOST_CHECK_CLOSE(coordinate.x(), 5.5, 0.001);
  BOOST_CHECK_CLOSE(coordinate.y(), 6.6, 0.001);
}
