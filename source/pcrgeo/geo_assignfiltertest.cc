#define BOOST_TEST_MODULE pcraster geo assign_filter
#include <boost/test/unit_test.hpp>
#include "geo_assignfilter.h"
#include "geo_filterengine.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  // Create filter. Weight matrix won't be used.
  SimpleRaster<double> weights(5, 5, 1.0);
  AssignFilter filter(weights);

  // Create source raster.
  SimpleRaster<int> source(5, 5);
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

  // Destination raster.
  SimpleRaster<int> destination(5, 5);

  FilterEngine<int, int> engine(source, filter, destination);
  engine.calc();

  BOOST_CHECK(source == destination);
}
