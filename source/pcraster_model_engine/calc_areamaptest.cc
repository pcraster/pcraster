#define BOOST_TEST_MODULE pcraster model_engine areamap
#include <boost/test/unit_test.hpp>
#include "geo_rasterspace.h"
#include "PCRasterXSD.h"
#include "calc_areamap.h"


BOOST_AUTO_TEST_CASE(testInit)
{
  using namespace calc;

  geo::RasterSpace in(3,10,5);

  AreaMap amIn(in);

  std::auto_ptr<pcrxml::CheckContext> cc(amIn.createXMLContext ());

  AreaMap amOut1(amIn.rasterSpace());

  // not possible anymore pcrxml::AreaMap != pcrxml::AreaMapScript
  // AreaMap amOut2(cc->areaMap().get());

  BOOST_CHECK(in == amOut1.rasterSpace());
  // BOOST_CHECK(in == amOut2.rasterSpace());
}
