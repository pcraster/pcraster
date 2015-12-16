#define BOOST_TEST_MODULE pcraster geo average_filter
#include <boost/test/unit_test.hpp>
#include "geo_averagefilter.h"
#include "geo_filterengine.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace geo;

  //  1  2  3  4  5
  //  6  7  8  9 10
  // 11 12 13 14 15
  // 16 17 MV 19 20
  // 21 22 23 24 25

  {
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
    pcr::setMV(source.cell(3, 2));
    source.cell(3, 3) = 19;
    source.cell(3, 4) = 20;
    source.cell(4, 0) = 21;
    source.cell(4, 1) = 22;
    source.cell(4, 2) = 23;
    source.cell(4, 3) = 24;
    source.cell(4, 4) = 25;

    // Filter for calculating average within kernel.
    SimpleRaster<double> weights(3, 3, 1.0);
    AverageFilter filter(weights);

    // Destination raster.
    SimpleRaster<double> destination(5, 5);

    FilterEngine<double, double> engine(source, filter, destination);
    engine.calc();

    BOOST_CHECK(destination.cell(0, 0) == 4.0);
    BOOST_CHECK(destination.cell(0, 1) == 4.5);
    BOOST_CHECK(destination.cell(0, 2) == 5.5);
    BOOST_CHECK(destination.cell(0, 3) == 6.5);
    BOOST_CHECK(destination.cell(0, 4) == 7.0);

    BOOST_CHECK(destination.cell(1, 0) == 6.5);
    BOOST_CHECK(destination.cell(1, 1) == 7.0);
    BOOST_CHECK(destination.cell(1, 2) == 8.0);
    BOOST_CHECK(destination.cell(1, 3) == 9.0);
    BOOST_CHECK(destination.cell(1, 4) == 9.5);

    BOOST_CHECK(destination.cell(2, 0) == 11.5);
    BOOST_CHECK(destination.cell(2, 1) == 11.25);
    BOOST_CHECK(destination.cell(2, 2) == 12.375);
    BOOST_CHECK(destination.cell(2, 3) == 13.5);
    BOOST_CHECK(destination.cell(2, 4) == 14.5);


    BOOST_CHECK(destination.cell(3, 0) == 16.5);
    BOOST_CHECK(destination.cell(3, 1) == 16.875);
    BOOST_CHECK(pcr::isMV(destination.cell(3, 2)));
    BOOST_CHECK(destination.cell(3, 3) == 19.125);
    BOOST_CHECK(destination.cell(3, 4) == 19.5);

    BOOST_CHECK(destination.cell(4, 0) == 19.0);
    BOOST_CHECK(destination.cell(4, 1) == 19.8);
    BOOST_CHECK(destination.cell(4, 2) == 21);
    BOOST_CHECK(destination.cell(4, 3) == 22.2);
    BOOST_CHECK(destination.cell(4, 4) == 22);
  }

  {
    // Create raster:
    // MV MV MV
    // MV MV  6
    // MV MV MV
    SimpleRaster<double> source(3, 3);
    pcr::setMV(source.cell(0, 0));
    pcr::setMV(source.cell(0, 1));
    pcr::setMV(source.cell(0, 2));
    pcr::setMV(source.cell(1, 0));
    pcr::setMV(source.cell(1, 1));
    source.cell(1, 2) = 6.0;
    pcr::setMV(source.cell(2, 0));
    pcr::setMV(source.cell(2, 1));
    pcr::setMV(source.cell(2, 2));

    // Filter for calculating average within kernel.
    SimpleRaster<double> weights(3, 3, 1.0);
    AverageFilter filter(weights);

    // Destination raster.
    SimpleRaster<double> destination(3, 3);

    FilterEngine<double, double> engine(source, filter, destination);
    engine.calc();

    BOOST_CHECK(pcr::isMV(destination.cell(0, 0)));
    BOOST_CHECK(pcr::isMV(destination.cell(0, 1)));
    BOOST_CHECK(pcr::isMV(destination.cell(0, 2)));
    BOOST_CHECK(pcr::isMV(destination.cell(1, 0)));
    BOOST_CHECK(pcr::isMV(destination.cell(1, 1)));
    BOOST_CHECK(destination.cell(1, 2) == 6.0);
    BOOST_CHECK(pcr::isMV(destination.cell(2, 0)));
    BOOST_CHECK(pcr::isMV(destination.cell(2, 1)));
    BOOST_CHECK(pcr::isMV(destination.cell(2, 2)));
  }

  {
    // Create raster:
    // MV MV MV
    // MV MV  6
    // MV MV MV
    SimpleRaster<double> source(3, 3);
    pcr::setMV(source.cell(0, 0));
    pcr::setMV(source.cell(0, 1));
    pcr::setMV(source.cell(0, 2));
    pcr::setMV(source.cell(1, 0));
    pcr::setMV(source.cell(1, 1));
    source.cell(1, 2) = 6.0;
    pcr::setMV(source.cell(2, 0));
    pcr::setMV(source.cell(2, 1));
    pcr::setMV(source.cell(2, 2));

    // Calculate average values for current and cell to the right.
    // 0 0 0
    // 0 1 1
    // 0 0 0
    SimpleRaster<double> weights(3, 3, 0.0);
    weights.cell(1, 1) = 1.0;
    weights.cell(1, 2) = 1.0;
    AverageFilter filter(weights);

    // Destination raster.
    SimpleRaster<double> destination(3, 3);

    FilterEngine<double, double> engine(source, filter, destination);
    engine.calc();

    BOOST_CHECK(pcr::isMV(destination.cell(0, 0)));
    BOOST_CHECK(pcr::isMV(destination.cell(0, 1)));
    BOOST_CHECK(pcr::isMV(destination.cell(0, 2)));
    BOOST_CHECK(pcr::isMV(destination.cell(1, 0)));
    BOOST_CHECK(pcr::isMV(destination.cell(1, 1)));
    BOOST_CHECK(destination.cell(1, 2) == 6.0);
    BOOST_CHECK(pcr::isMV(destination.cell(2, 0)));
    BOOST_CHECK(pcr::isMV(destination.cell(2, 1)));
    BOOST_CHECK(pcr::isMV(destination.cell(2, 2)));
  }
}
