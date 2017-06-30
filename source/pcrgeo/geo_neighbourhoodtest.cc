#define BOOST_TEST_MODULE pcraster geo neighbourhood
#include <boost/test/unit_test.hpp>
#include "geo_neighbourhood.h"


class NeighbourhoodWrapper : public geo::Neighbourhood {
public:
   NeighbourhoodWrapper(double fromRadius, double toRadius) : Neighbourhood(fromRadius, toRadius) {
   };
};

BOOST_AUTO_TEST_CASE(random_cell_locations)
{
  using namespace geo;

  {
    // If the number of cells we request equals the number of non zero values
    // in the neighbourhood, than we excpect to get all cells within the
    // neighbourhood back.
    std::vector<LinearLoc> locations;
    RasterDim space(3, 3); // 3x3 raster
    NeighbourhoodWrapper neighbourhood(0, 1); // 3x3 neighbourhood
    PRECOND(neighbourhood.nrRows() == 3);
    PRECOND(neighbourhood.nrCols() == 3);
    neighbourhood.fill(1.0); // all cells in neighbourhood participate
    CellLoc cell(1, 1); // center of raster

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == neighbourhood.nrCells());

    // Neigbourhood with one zero.
    locations.clear();
    neighbourhood.fill(1.0);
    neighbourhood.cell(0, 0) = 0.0;

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == neighbourhood.nrCells() - 1);

    // Neigbourhood with only zero's.
    locations.clear();
    neighbourhood.fill(0.0);

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == 0);

    // Neighbourhood at upper left of raster.
    locations.clear();
    neighbourhood.fill(1.0);
    cell = CellLoc(0, 0);

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == 4);

    // Neighbourhood at upper right of raster.
    locations.clear();
    neighbourhood.fill(1.0);
    cell = CellLoc(0, 2);

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == 4);

    // Neighbourhood at lower right of raster.
    locations.clear();
    neighbourhood.fill(1.0);
    cell = CellLoc(2, 2);

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == 4);

    // Neighbourhood at lower left of raster.
    locations.clear();
    neighbourhood.fill(1.0);
    cell = CellLoc(2, 0);

    randomCellLocations(locations, neighbourhood.nrCells(), space,
         neighbourhood, cell);
    BOOST_CHECK(locations.size() == 4);
  }
}
