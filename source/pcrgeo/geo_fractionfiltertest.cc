#define BOOST_TEST_MODULE pcraster geo fraction_filter
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "geo_mooreneighbourhood.h"
#include "geo_filterengine.h"
#include "geo_fractionfilter.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  // Fraction of true cells in a filter.

  // 1 0 0
  // 0 1 MV
  // 0 0 1
  SimpleRaster<UINT1> source(3, 3);
  source.cell(0) = 1;
  source.cell(1) = 0;
  source.cell(2) = 0;
  source.cell(3) = 0;
  source.cell(4) = 1;
  source.setMV(5);
  source.cell(6) = 0;
  source.cell(7) = 0;
  source.cell(8) = 1;

  SquareNeighbourhood weights(1);
  FractionFilter<UINT1> filter(weights, 1);

  // 2/4 2/5 1/3
  // 2/6 3/8 MV
  // 1/4 2/5 2/3
  SimpleRaster<REAL8> destination(3, 3);

  FilterEngine<UINT1, REAL8> engine(source, filter, destination);
  engine.calc();

  BOOST_CHECK(dal::comparable(destination.cell(0), 2.0 / 4.0));
  BOOST_CHECK(dal::comparable(destination.cell(1), 2.0 / 5.0));
  BOOST_CHECK(dal::comparable(destination.cell(2), 1.0 / 3.0));
  BOOST_CHECK(dal::comparable(destination.cell(3), 2.0 / 6.0));
  BOOST_CHECK(dal::comparable(destination.cell(4), 3.0 / 8.0));
  BOOST_CHECK(destination.isMV(5));
  BOOST_CHECK(dal::comparable(destination.cell(6), 1.0 / 4.0));
  BOOST_CHECK(dal::comparable(destination.cell(7), 2.0 / 5.0));
  BOOST_CHECK(dal::comparable(destination.cell(8), 2.0 / 3.0));
}
