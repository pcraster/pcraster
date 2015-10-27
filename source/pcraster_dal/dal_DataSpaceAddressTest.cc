#define BOOST_TEST_MODULE pcraster dal data_space_address
#include <boost/test/unit_test.hpp>
#include "dal_DataSpaceAddress.h"
#include "dal_SpatialCoordinate.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(copy)
{
  using namespace dal;

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
