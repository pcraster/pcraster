#define BOOST_TEST_MODULE pcraster model_engine areamap
#include <boost/test/unit_test.hpp>
#include "geo_rasterspace.h"
#include "PCRasterXSD.h"
#include "calc_areamap.h"


BOOST_AUTO_TEST_CASE(testInit)
{
  using namespace calc;

  geo::RasterSpace const in(3,10,5);

  AreaMap const amIn(in);

  std::unique_ptr<pcrxml::CheckContext> const cc(amIn.createXMLContext ());

  AreaMap const amOut1(amIn.rasterSpace());

  // not possible anymore pcrxml::AreaMap != pcrxml::AreaMapScript
  // AreaMap amOut2(cc->areaMap().get());

  BOOST_CHECK(in == amOut1.rasterSpace());
  // BOOST_CHECK(in == amOut2.rasterSpace());
}
