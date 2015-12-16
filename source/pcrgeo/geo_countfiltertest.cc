#define BOOST_TEST_MODULE pcraster geo count_filter_test
#include <boost/test/unit_test.hpp>
#include "geo_countfilter.h"
#include "geo_filterengine.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  // Create source raster.
#ifdef __x86_64__
  typedef unsigned int count_t;
#else
  typedef size_t count_t;
#endif
  SimpleRaster<count_t> source(3, 3);
  source.cell(0, 0) = 1;
  source.cell(0, 1) = 2;
  source.cell(0, 2) = 3;
  source.cell(1, 0) = 4;
  source.cell(1, 1) = 5;
  source.cell(1, 2) = 6;
  source.cell(2, 0) = 7;
  source.cell(2, 1) = 8;
  source.cell(2, 2) = 9;

  SimpleRaster<double> weights(3, 3, 1.0);
  CountFilter<count_t, double> filter(weights, 4);
  SimpleRaster<double> destination(3, 3);
  FilterEngine<count_t, double> engine(source, filter, destination);

  engine.calc();

  BOOST_CHECK(destination.cell(0, 0) == 1.0);
  BOOST_CHECK(destination.cell(0, 1) == 1.0);
  BOOST_CHECK(destination.cell(0, 2) == 0.0);
  BOOST_CHECK(destination.cell(1, 0) == 1.0);
  BOOST_CHECK(destination.cell(1, 1) == 1.0);
  BOOST_CHECK(destination.cell(1, 2) == 0.0);
  BOOST_CHECK(destination.cell(2, 0) == 1.0);
  BOOST_CHECK(destination.cell(2, 1) == 1.0);
  BOOST_CHECK(destination.cell(2, 2) == 0.0);

  // see __x86_64__
  // TODO Suse problem: 8 byte isMV template instantion for unsigned long  
  bool size_tRequires8Byteonx86_64=false;
  BOOST_WARN(size_tRequires8Byteonx86_64);
}
