#define BOOST_TEST_MODULE pcraster geo csf_map
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "com_pathinfo.h"
#include "geo_csfmap.h"


struct Fixture
{

    Fixture()
    {
        d_map1 = new geo::CSFMap("map1.map",11, 12, VS_SCALAR, PT_YINCT2B,
            3.0, 14.0, 1.0, 5.0);
    }

    ~Fixture()
    {
      delete d_map1;
      remove("map1.map");
    }

    geo::CSFMap* d_map1;

};


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace geo;

 // illegal angle
 CSFMap *map2(0);
 bool failureExpected=false;
 try {
    map2 = new CSFMap("csfmap2.map",11, 12, VS_SCALAR, PT_YINCT2B, 3.0, 14.0, 15.0, 5.0);
 } catch(const com::FileError& e) {
  const char *msgExpect =
    "File 'csfmap2.map': error creating raster: Angle < -0.5 pi or > 0.5 pi\n";
  BOOST_CHECK(e.messages() == msgExpect);
  failureExpected=true;
 }
 BOOST_CHECK(failureExpected);
 delete map2;
 BOOST_CHECK(!com::PathInfo("csfmap2.map").exists());
}


BOOST_FIXTURE_TEST_SUITE(csf_map, Fixture)

BOOST_AUTO_TEST_CASE(nr_rows)
{
  using namespace geo;

  BOOST_CHECK(d_map1->nrRows() == 11);
}


BOOST_AUTO_TEST_CASE(nr_cols)
{
  using namespace geo;

  BOOST_CHECK(d_map1->nrCols() == 12);
}


BOOST_AUTO_TEST_CASE(nr_cells)
{
  using namespace geo;

  BOOST_CHECK(d_map1->nrCells() == 11 * 12);
}

BOOST_AUTO_TEST_SUITE_END()
