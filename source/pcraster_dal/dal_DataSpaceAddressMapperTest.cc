#define BOOST_TEST_MODULE pcraster dal data_space_address_mapper
#include <boost/test/unit_test.hpp>
#include "dal_DataSpaceAddressMapper.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  {
    std::set<std::string> scenarios;
    std::vector<size_t> timeSteps, rows, cols;
    scenarios.insert("aap");
    timeSteps.push_back(1);
    timeSteps.push_back(100);
    timeSteps.push_back(1);
    rows.push_back(1);
    rows.push_back(100);
    rows.push_back(1);
    cols.push_back(1);
    cols.push_back(100);
    cols.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Scenarios, scenarios));
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(Space, rows));
    space.addDimension(Dimension(Space, cols));

    DataSpaceAddressMapper mapper(space);

    DataSpaceAddress address = space.address();
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 50);
    address.setCoordinate<size_t>(2, 12);
    address.setCoordinate<size_t>(3, 13);

    BOOST_CHECK_EQUAL(mapper.toString(address, 0), "aap");
    BOOST_CHECK_EQUAL(mapper.toString(address, 1), "50");
    BOOST_CHECK_EQUAL(mapper.toString(address, 2), "12");
    BOOST_CHECK_EQUAL(mapper.toString(address, 3), "13");
    BOOST_CHECK_EQUAL(mapper.toString(address), "/aap/50/12/13");
  }
}
