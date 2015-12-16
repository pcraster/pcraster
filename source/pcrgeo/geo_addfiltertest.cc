#define BOOST_TEST_MODULE pcraster geo add_filter
#include <boost/test/unit_test.hpp>
#include "geo_addfilter.h"
#include "geo_filterengine.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  // Create source raster.
  SimpleRaster<double> source(5, 5);
  source.cell(0, 0) = 1;
  source.cell(0, 1) = 2;
  source.cell(0, 2) = 3;
  source.cell(0, 3) = 4;
  source.cell(0, 4) = 5;
  source.cell(1, 0) = 6;
  source.cell(1, 1) = 7;
  source.cell(1, 2) = 8;
  source.cell(1, 3) = 9;
  source.cell(1, 4) = 10;
  source.cell(2, 0) = 11;
  source.cell(2, 1) = 12;
  source.cell(2, 2) = 13;
  source.cell(2, 3) = 14;
  source.cell(2, 4) = 15;
  source.cell(3, 0) = 16;
  source.cell(3, 1) = 17;
  source.cell(3, 2) = 18;
  source.cell(3, 3) = 19;
  source.cell(3, 4) = 20;
  source.cell(4, 0) = 21;
  source.cell(4, 1) = 22;
  source.cell(4, 2) = 23;
  source.cell(4, 3) = 24;
  source.cell(4, 4) = 25;

  // Filter for adding all values within the kernel.
  SimpleRaster<double> weights(3, 3, 1.0);
  AddFilter filter(weights);

  // Destination raster.
  SimpleRaster<double> destination(5, 5);

  FilterEngine<double, double> engine(source, filter, destination);

  // ---------------------------------------------------------------------------
  engine.calc();

  BOOST_CHECK(destination.cell(0, 0) == 16.0);
  BOOST_CHECK(destination.cell(0, 1) == 27.0);
  BOOST_CHECK(destination.cell(0, 2) == 33.0);
  BOOST_CHECK(destination.cell(0, 3) == 39.0);
  BOOST_CHECK(destination.cell(0, 4) == 28.0);
  BOOST_CHECK(destination.cell(1, 0) == 39.0);
  BOOST_CHECK(destination.cell(1, 1) == 63.0);
  BOOST_CHECK(destination.cell(1, 2) == 72.0);
  BOOST_CHECK(destination.cell(1, 3) == 81.0);
  BOOST_CHECK(destination.cell(1, 4) == 57.0);
  BOOST_CHECK(destination.cell(2, 0) == 69.0);
  BOOST_CHECK(destination.cell(2, 1) == 108.0);
  BOOST_CHECK(destination.cell(2, 2) == 117.0);
  BOOST_CHECK(destination.cell(2, 3) == 126.0);
  BOOST_CHECK(destination.cell(2, 4) == 87.0);
  BOOST_CHECK(destination.cell(3, 0) == 99.0);
  BOOST_CHECK(destination.cell(3, 1) == 153.0);
  BOOST_CHECK(destination.cell(3, 2) == 162.0);
  BOOST_CHECK(destination.cell(3, 3) == 171.0);
  BOOST_CHECK(destination.cell(3, 4) == 117.0);
  BOOST_CHECK(destination.cell(4, 0) == 76.0);
  BOOST_CHECK(destination.cell(4, 1) == 117.0);
  BOOST_CHECK(destination.cell(4, 2) == 123.0);
  BOOST_CHECK(destination.cell(4, 3) == 129.0);
  BOOST_CHECK(destination.cell(4, 4) == 88.0);

  // ---------------------------------------------------------------------------
  // Test weighting factors.
  filter.cell(0, 0) = 1.0;
  filter.cell(0, 1) = 2.0;
  filter.cell(0, 2) = 3.0;
  filter.cell(1, 0) = 4.0;
  filter.cell(1, 1) = 5.0;
  filter.cell(1, 2) = 6.0;
  filter.cell(2, 0) = 7.0;
  filter.cell(2, 1) = 8.0;
  filter.cell(2, 2) = 9.0;

  engine.calc();

  BOOST_CHECK(destination.cell(0, 0) == 128.0);
  BOOST_CHECK(destination.cell(0, 1) == 202.0);
  BOOST_CHECK(destination.cell(0, 4) == 184.0);
  BOOST_CHECK(destination.cell(1, 0) == 276.0);
  BOOST_CHECK(destination.cell(1, 1) == 411.0);
  BOOST_CHECK(destination.cell(1, 4) == 318.0);
  BOOST_CHECK(destination.cell(4, 0) == 320.0);
  BOOST_CHECK(destination.cell(4, 1) == 436.0);
  BOOST_CHECK(destination.cell(4, 4) == 280.0);
}
