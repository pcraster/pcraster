#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_NEIGHBOURHOODTEST
#include "geo_neighbourhoodtest.h"
#define INCLUDED_GEO_NEIGHBOURHOODTEST
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
#ifndef INCLUDED_GEO_NEIGHBOURHOOD
#include "geo_neighbourhood.h"
#define INCLUDED_GEO_NEIGHBOURHOOD
#endif



/*!
  \file
  This file contains the implementation of the NeighbourhoodTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace geo {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC NEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*NeighbourhoodTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<NeighbourhoodTest> instance(new NeighbourhoodTest());

  suite->add(BOOST_CLASS_TEST_CASE(&NeighbourhoodTest::testRandomCellLocations, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF NEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! ctor
NeighbourhoodTest::NeighbourhoodTest(
         )
{
}



//! setUp
void NeighbourhoodTest::setUp()
{
}



//! tearDown
void NeighbourhoodTest::tearDown()
{
}



void NeighbourhoodTest::testRandomCellLocations()
{
  {
    // If the number of cells we request equals the number of non zero values
    // in the neighbourhood, than we excpect to get all cells within the
    // neighbourhood back.
    std::vector<LinearLoc> locations;
    RasterDim space(3, 3); // 3x3 raster
    Neighbourhood neighbourhood(0, 1); // 3x3 neighbourhood
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



} // namespace geo

