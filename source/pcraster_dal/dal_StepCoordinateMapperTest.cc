#define BOOST_TEST_MODULE pcraster dal step_coordinate_mapper
#include <boost/test/unit_test.hpp>
#include "dal_DataSpace.h"
#include "dal_StepCoordinateMapper.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

  {
    // Catsop:
    size_t nrCols = 80;
    double cellSize = 10.0;
    double west = 182140.0;
    StepCoordinateMapper mapper(1, nrCols,
         west + 0.5 * cellSize, west + (double(nrCols) - 0.5) * cellSize,
         SetToMissingValue);

    std::vector<size_t> cols;
    cols.push_back(1);
    cols.push_back(100);
    cols.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Space, cols));
    DataSpaceAddress address = space.address();

    address.setCoordinate<size_t>(0, 1);
    BOOST_CHECK_EQUAL(mapper.toString(space, address, 0),  "182145");

    address.setCoordinate<size_t>(0, nrCols);
    BOOST_CHECK_EQUAL(mapper.toString(space, address, 0),  "182935");
  }

  //  1 "10-2-2005, 18:15: volcano_1"
  //  2 "11-2-2005, 24:15: volcano_1"
  //  3 "11-2-2005, 06:15: volcano_1"
  //  4 "11-2-2005, 12:15: volcano_1"
  //  5 "11-2-2005, 18:15: volcano_2, lava_1"
  //  6 "12-2-2005, 24:15: volcano_2, lava_2"
  //  7 "12-2-2005, 06:15: volcano_2, lava_3"
  //  8 "12-2-2005, 12:15: volcano_2, lava_4"
  //  9 "12-2-2005, 18:15: volcano_3, lava_5"
  // 10 "13-2-2005, 24:15: volcano_3, lava_6"
  // 11 "13-2-2005, 06:15: volcano_3, lava_7"
  // 12 "13-2-2005, 12:15: volcano_3, lava_8"
  // 13 "13-2-2005, 18:15: volcano_3, lava_9"
  {
    // 1 - 13 -> 1 - 4
    StepCoordinateMapper mapper(1, 9, 1, 3, UsePrevious);

    std::vector<size_t> steps;
    steps.push_back(1);
    steps.push_back(13);
    steps.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Time, steps));
    DataSpaceAddress address = space.address();

    address.setCoordinate<size_t>(0, 1);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(1));

    address.setCoordinate<size_t>(0, 2);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(1));

    address.setCoordinate<size_t>(0, 3);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(1));

    address.setCoordinate<size_t>(0, 4);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(1));

    address.setCoordinate<size_t>(0, 5);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(2));

    address.setCoordinate<size_t>(0, 8);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(2));

    address.setCoordinate<size_t>(0, 9);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(3));
  }

  {
    // 5 - 13 -> 1 - 9
    StepCoordinateMapper mapper(5, 13, 1, 9, UsePrevious);

    std::vector<size_t> steps;
    steps.push_back(5);
    steps.push_back(13);
    steps.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Time, steps));
    DataSpaceAddress address = space.address();

    address.setCoordinate<size_t>(0, 4);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK(!address.isValid(0));

    address.setCoordinate<size_t>(0, 5);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(1));

    address.setCoordinate<size_t>(0, 6);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(2));

    address.setCoordinate<size_t>(0, 7);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(3));

    address.setCoordinate<size_t>(0, 13);
    mapper.mapToDestination(space, address, 0);
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0),  size_t(9));
  }
}
