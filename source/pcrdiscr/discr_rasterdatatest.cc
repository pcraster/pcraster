#define BOOST_TEST_MODULE pcraster discr raster_data
#include <boost/test/unit_test.hpp>
#include "pcrtypes.h"
#include "discr_rasterdata.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace discr;

  size_t nrRows = 3;
  size_t nrCols = 4;
  double cellSize = 1.5;
  double west = 1.0;
  double north = 0.0;

  Raster raster(nrRows, nrCols, cellSize, west, north);

  {
    RasterData<REAL4> dem(&raster);

    for(size_t i = 0; i < raster.nrCells(); ++i) {
      BOOST_CHECK(!dem.isMV(i));
      BOOST_CHECK(dem.cell(i) == REAL4());
    }
  }

  {
    RasterData<REAL4> dem(&raster, 5.0);

    for(size_t i = 0; i < raster.nrCells(); ++i) {
      BOOST_CHECK(!dem.isMV(i));
      BOOST_CHECK(dem.cell(i) == 5.0);
    }
  }

  {
    REAL4 values[12] = {
         1.0, 3.0, 5.0, 7.0,
         2.0, 4.0, 6.0, 8.0,
         3.0, 5.0, 7.0, 9.0};
    pcr::setMV(values[6]);

    RasterData<REAL4> dem(&raster, values);

    for(size_t i = 0; i < raster.nrCells(); ++i) {
      if(i != 6) {
        BOOST_CHECK(!dem.isMV(i));
      }
    }

    BOOST_CHECK(dem.cell(0) == 1.0);
    BOOST_CHECK(dem.cell(4) == 2.0);
    BOOST_CHECK(dem.isMV(6));
    BOOST_CHECK(dem.cell(8) == 3.0);
    BOOST_CHECK(dem.cell(11) == 9.0);

    RasterData<REAL4> copyOfDem(dem);
    for(size_t i = 0; i < raster.nrCells(); ++i) {
      if(i != 6) {
        BOOST_CHECK(!dem.isMV(i));
      }
    }

    BOOST_CHECK(dem.cell(0) == 1.0);
    BOOST_CHECK(dem.cell(4) == 2.0);
    BOOST_CHECK(dem.isMV(6));
    BOOST_CHECK(dem.cell(8) == 3.0);
    BOOST_CHECK(dem.cell(11) == 9.0);
  }
}


BOOST_AUTO_TEST_CASE(equals)
{
  using namespace discr;

  Raster raster(3, 4, 1.5, 1.0, 0.0);

  REAL4 values[12] = {
       1.0, 3.0, 5.0, 7.0,
       2.0, 4.0, 6.0, 8.0,
       3.0, 5.0, 7.0, 9.0};
  pcr::setMV(values[6]);

  RasterData<REAL4> dem1(&raster, values);
  RasterData<REAL4> dem2(&raster, values);
  BOOST_CHECK(dem1 == dem1);
  BOOST_CHECK(dem1 == dem2);
  BOOST_CHECK(dem2 == dem1);

  dem2.cell(0) = 10.0;
  BOOST_CHECK(dem1 != dem2);
  BOOST_CHECK(dem2 != dem1);

  RasterData<REAL4> defaultRaster(&raster);
  BOOST_CHECK(defaultRaster == defaultRaster);
  BOOST_CHECK(dem1 != defaultRaster);
  BOOST_CHECK(defaultRaster != dem1);

  for(size_t i = 0; i < 12; ++i) {
    pcr::setMV(values[i]);
  }

  RasterData<REAL4> mvRaster(&raster, values);

  BOOST_CHECK(dem1 != mvRaster);
  dem1.setAllMV();
  BOOST_CHECK(dem1 == mvRaster);
}


BOOST_AUTO_TEST_CASE(assignment_operator)
{
  using namespace discr;

  Raster raster(3, 4, 1.5, 1.0, 0.0);
  RasterData<REAL4> dem1(&raster);

  RasterData<REAL4> dem2(&raster, 5.0);
  BOOST_CHECK(dem1 != dem2);

  dem1 = 5.0;
  BOOST_CHECK(dem1 == dem2);

  dem1 = RasterData<REAL4>(&raster);
  BOOST_CHECK(dem1 != dem2);

  dem1 = dem2;
  BOOST_CHECK(dem1 == dem2);
}
