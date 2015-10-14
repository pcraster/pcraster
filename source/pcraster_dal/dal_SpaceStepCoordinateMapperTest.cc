#define BOOST_TEST_MODULE pcraster dal space_step_coordinate_mapper
#include <boost/test/unit_test.hpp>
#include "dal_DataSpace.h"
#include "dal_SpaceStepCoordinateMapper.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

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
