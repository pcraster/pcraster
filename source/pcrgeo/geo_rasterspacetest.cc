#define BOOST_TEST_MODULE pcraster geo raster_space
#include <boost/test/unit_test.hpp>
#include <sstream>
#include "com_exception.h"
#include "geo_rasterspace.h"


struct Fixture
{

    Fixture()
    {
      using namespace geo;

      d_rs1 = new RasterSpace(11, 12, 13.0, 14.0, 15.0);
    }

    ~Fixture()
    {
      delete d_rs1;
    }

    geo::RasterSpace* d_rs1;

};


BOOST_FIXTURE_TEST_SUITE(raster_space, Fixture)

BOOST_AUTO_TEST_CASE(equality)
{
  using namespace geo;

  BOOST_CHECK(RasterSpace(1, 2, 3.0, 4.0, 5.0, geo::YIncrB2T, 6.0) ==
            RasterSpace(1, 2, 3.0, 4.0, 5.0, geo::YIncrB2T, 6.0));
}


BOOST_AUTO_TEST_CASE(nr_rows)
{
  using namespace geo;

  BOOST_CHECK(d_rs1->nrRows() == 11);
}


BOOST_AUTO_TEST_CASE(nr_cols)
{
  using namespace geo;

  BOOST_CHECK(d_rs1->nrCols() == 12);
}


BOOST_AUTO_TEST_CASE(nr_cells)
{
  using namespace geo;

  BOOST_CHECK(d_rs1->nrCells() == 11 * 12);
}


BOOST_AUTO_TEST_CASE(quadrant)
{
  using namespace geo;

  {
    RasterSpace space(3, 3, 1.0, 0.0, 0.0, geo::YIncrT2B);
    BOOST_CHECK(space.quadrant(0.0, 0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0, 0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0, 3.0) == NorthWest);
    BOOST_CHECK(space.quadrant(0.0, 3.0) == NorthWest);

    BOOST_CHECK(space.quadrant(1.1, 1.1) == NorthWest);
    BOOST_CHECK(space.quadrant(1.9, 1.1) == NorthEast);
    BOOST_CHECK(space.quadrant(1.9, 1.9) == SouthEast);
    BOOST_CHECK(space.quadrant(1.1, 1.9) == SouthWest);

    BOOST_CHECK(space.quadrant(1.5, 1.5) == SouthEast);
  }

  {
    RasterSpace space(3, 3, 1.0, 0.0, 0.0, geo::YIncrB2T);
    BOOST_CHECK(space.quadrant(0.0,  0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0,  0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0, -3.0) == NorthWest);
    BOOST_CHECK(space.quadrant(0.0, -3.0) == NorthWest);

    BOOST_CHECK(space.quadrant(1.1, -1.1) == NorthWest);
    BOOST_CHECK(space.quadrant(1.9, -1.1) == NorthEast);
    BOOST_CHECK(space.quadrant(1.9, -1.9) == SouthEast);
    BOOST_CHECK(space.quadrant(1.1, -1.9) == SouthWest);

    BOOST_CHECK(space.quadrant(1.5, -1.5) == SouthEast);
  }
}


BOOST_AUTO_TEST_CASE(io)
{
  using namespace geo;

  bool stuffReadDoesntEqualStuffWritten;

  try {

    stuffReadDoesntEqualStuffWritten = false;

    std::stringstream s;

    RasterSpace rs1(11, 12, 13.0, 14.0, 15.0, geo::YIncrB2T);
    s << rs1;

    RasterSpace rs2;
    s >> rs2;

    BOOST_CHECK(rs1 == rs2);

  }
  catch(const com::BadStreamFormat& ) {
    stuffReadDoesntEqualStuffWritten = true;
  }

  BOOST_CHECK(!stuffReadDoesntEqualStuffWritten);



  try {

    stuffReadDoesntEqualStuffWritten = false;

    std::stringstream s;

    RasterSpace rs1(11, 12, 13.0, 14.0, 15.0, geo::YIncrB2T);
    s << "blabla" << rs1;

    RasterSpace rs2;
    s >> rs2;

  }
  catch(const com::BadStreamFormat& ) {
    stuffReadDoesntEqualStuffWritten = true;
  }

  BOOST_CHECK(stuffReadDoesntEqualStuffWritten);
}

BOOST_AUTO_TEST_SUITE_END()
