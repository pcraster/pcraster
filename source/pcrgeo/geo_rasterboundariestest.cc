#define BOOST_TEST_MODULE pcraster geo raster_boundaries
#include <boost/test/unit_test.hpp>
#define private public
#include "geo_rasterboundaries.h"


BOOST_AUTO_TEST_CASE(index_left)
{
  using namespace geo;

  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexLeft(0, 0) == 2);
  BOOST_CHECK(boundaries.indexLeft(0, 1) == 3);
  BOOST_CHECK(boundaries.indexLeft(1, 0) == 7);
  BOOST_CHECK(boundaries.indexLeft(1, 1) == 8);
  BOOST_CHECK(boundaries.indexLeft(2, 0) == 12);
  BOOST_CHECK(boundaries.indexLeft(2, 1) == 13);
}


BOOST_AUTO_TEST_CASE(index_top)
{
  using namespace geo;

  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexTop(0, 0) == 0);
  BOOST_CHECK(boundaries.indexTop(0, 1) == 1);
  BOOST_CHECK(boundaries.indexTop(1, 0) == 5);
  BOOST_CHECK(boundaries.indexTop(1, 1) == 6);
  BOOST_CHECK(boundaries.indexTop(2, 0) == 10);
  BOOST_CHECK(boundaries.indexTop(2, 1) == 11);
}


BOOST_AUTO_TEST_CASE(index_right)
{
  using namespace geo;

  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexRight(0, 0) == 3);
  BOOST_CHECK(boundaries.indexRight(0, 1) == 4);
  BOOST_CHECK(boundaries.indexRight(1, 0) == 8);
  BOOST_CHECK(boundaries.indexRight(1, 1) == 9);
  BOOST_CHECK(boundaries.indexRight(2, 0) == 13);
  BOOST_CHECK(boundaries.indexRight(2, 1) == 14);
}


BOOST_AUTO_TEST_CASE(index_bottom)
{
  using namespace geo;

  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexBottom(0, 0) == 5);
  BOOST_CHECK(boundaries.indexBottom(0, 1) == 6);
  BOOST_CHECK(boundaries.indexBottom(1, 0) == 10);
  BOOST_CHECK(boundaries.indexBottom(1, 1) == 11);
  BOOST_CHECK(boundaries.indexBottom(2, 0) == 15);
  BOOST_CHECK(boundaries.indexBottom(2, 1) == 16);
}


BOOST_AUTO_TEST_CASE(boundaries)
{
  using namespace geo;

  RasterBoundaries<size_t> boundaries(3, 2);

  boundaries.leftBoundary(0, 0) = 2;
  boundaries.leftBoundary(0, 1) = 3;
  boundaries.leftBoundary(1, 0) = 7;
  boundaries.leftBoundary(1, 1) = 8;
  boundaries.leftBoundary(2, 0) = 12;
  boundaries.leftBoundary(2, 1) = 13;

  boundaries.rightBoundary(0, 1) = 4;
  boundaries.rightBoundary(1, 1) = 9;
  boundaries.rightBoundary(2, 1) = 14;

  boundaries.topBoundary(0, 0) = 0;
  boundaries.topBoundary(0, 1) = 1;
  boundaries.topBoundary(1, 0) = 5;
  boundaries.topBoundary(1, 1) = 6;
  boundaries.topBoundary(2, 0) = 10;
  boundaries.topBoundary(2, 1) = 11;

  boundaries.bottomBoundary(2, 0) = 15;
  boundaries.bottomBoundary(2, 1) = 16;

  BOOST_CHECK(boundaries.leftBoundary(0, 0) == 2);
  BOOST_CHECK(boundaries.topBoundary(0, 0) == 0);
  BOOST_CHECK(boundaries.rightBoundary(0, 0) == 3);
  BOOST_CHECK(boundaries.bottomBoundary(0, 0) == 5);

  BOOST_CHECK(boundaries.leftBoundary(1, 1) == 8);
  BOOST_CHECK(boundaries.topBoundary(1, 1) == 6);
  BOOST_CHECK(boundaries.rightBoundary(1, 1) == 9);
  BOOST_CHECK(boundaries.bottomBoundary(1, 1) == 11);

  BOOST_CHECK(boundaries.rightBoundary(2, 0) == boundaries.leftBoundary(2, 1));
  BOOST_CHECK(boundaries.topBoundary(2, 0) == boundaries.bottomBoundary(1, 0));
}
