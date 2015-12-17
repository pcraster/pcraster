#define BOOST_TEST_MODULE pcraster discr raster
#include <boost/test/unit_test.hpp>
#include "discr_raster.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace discr;

  size_t nrRows = 3;
  size_t nrCols = 4;
  double cellSize = 1.5;
  double west = 1.0;
  double north = 0.0;

  {
    Raster raster(nrRows, nrCols);
    BOOST_CHECK(raster.nrRows() == nrRows);
    BOOST_CHECK(raster.nrCols() == nrCols);
    BOOST_CHECK(raster.nrCells() == nrRows * nrCols);
    BOOST_CHECK(raster.cellSize() == 1.0);
    BOOST_CHECK(raster.west() == 0.0);
    BOOST_CHECK(raster.north() == 0.0);
  }

  {
    Raster raster(nrRows, nrCols, cellSize, west, north);
    BOOST_CHECK(raster.nrRows() == nrRows);
    BOOST_CHECK(raster.nrCols() == nrCols);
    BOOST_CHECK(raster.nrCells() == nrRows * nrCols);
    BOOST_CHECK(raster.cellSize() == cellSize);
    BOOST_CHECK(raster.west() == west);
    BOOST_CHECK(raster.north() == north);
  }
}


BOOST_AUTO_TEST_CASE(equals)
{
  using namespace discr;

  Raster raster1(3, 4, 1.5, 1.0, 0.0);
  Raster raster2(3, 4, 1.5, 1.0, 1.0);
  BOOST_CHECK(raster1 == raster1);
  BOOST_CHECK(raster2 == raster2);
  BOOST_CHECK(raster1 != raster2);
  BOOST_CHECK(raster2 != raster1);
}
