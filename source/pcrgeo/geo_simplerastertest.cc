#define BOOST_TEST_MODULE pcraster geo simple_raster
#include <boost/test/unit_test.hpp>
#include "geo_simpleraster.h"


struct Fixture
{

    Fixture()
    {
      using namespace geo;

      // Empty raster.
      d_raster1 = new SimpleRaster<int>(0, 0);

      // 5 x 5 raster with cell values 8.
      d_raster2 = new SimpleRaster<int>(5, 5, 8);

      // 5 x 5 raster with undefined cell values.
      d_raster3 = new SimpleRaster<int>(5, 5);
    }

    ~Fixture()
    {
      delete d_raster3;
      delete d_raster2;
      delete d_raster1;
    }

    geo::SimpleRaster<int>* d_raster1;
    geo::SimpleRaster<int>* d_raster2;
    geo::SimpleRaster<int>* d_raster3;

};


BOOST_FIXTURE_TEST_SUITE(simple_raster, Fixture)

BOOST_AUTO_TEST_CASE(properties)
{
  using namespace geo;

  BOOST_CHECK(d_raster1->nrRows() == 0);
  BOOST_CHECK(d_raster1->nrCols() == 0);
  BOOST_CHECK(d_raster1->nrCells() == 0);

  BOOST_CHECK(d_raster2->nrRows() == 5);
  BOOST_CHECK(d_raster2->nrCols() == 5);
  BOOST_CHECK(d_raster2->nrCells() == 25);
}


BOOST_AUTO_TEST_CASE(contents)
{
  using namespace geo;

  BOOST_CHECK(d_raster2->end() - d_raster2->begin() ==
                   static_cast<int>(d_raster2->nrCells()));

  for(SimpleRaster<int>::const_iterator it = d_raster2->begin();
                   it != d_raster2->end(); ++it) {
    BOOST_CHECK(*it == 8);
  }

  for(size_t r = 0; r < d_raster2->nrRows(); ++r) {
    for(size_t c = 0; c < d_raster2->nrCols(); ++c) {
      BOOST_CHECK(d_raster2->cell(r, c) == 8);
    }
  }

  for(size_t r = 0; r < d_raster3->nrRows(); ++r) {
    for(size_t c = 0; c < d_raster3->nrCols(); ++c) {
      d_raster3->cell(r, c) = (r + 1) * (c + 1);
    }
  }

  for(size_t r = 0; r < d_raster3->nrRows(); ++r) {
    for(size_t c = 0; c < d_raster3->nrCols(); ++c) {
      BOOST_CHECK(d_raster3->cell(r, c) == static_cast<int>((r + 1) * (c + 1)));
    }
  }
}


BOOST_AUTO_TEST_CASE(assignment)
{
  using namespace geo;

  SimpleRaster<int>::const_iterator it2, it3;

  bool result = true;

  for(it2 = d_raster2->begin(), it3 = d_raster3->begin();
                   it2 != d_raster2->end() && it3 != d_raster3->end();
                   ++it2, ++it3) {
    if(*it2 != *it3) {
      result = false;
      break;
    }
  }

  // The rasters should be different.
  BOOST_CHECK(!result);

  *d_raster2 = *d_raster3;
  result = true;

  for(it2 = d_raster2->begin(), it3 = d_raster3->begin();
                   it2 != d_raster2->end() && it3 != d_raster3->end();
                   ++it2, ++it3) {
    if(*it2 != *it3) {
      result = false;
      break;
    }
  }

  // The rasters should be equal.
  BOOST_CHECK(result);
}

BOOST_AUTO_TEST_SUITE_END()
