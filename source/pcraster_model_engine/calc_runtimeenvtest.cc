#define BOOST_TEST_MODULE pcraster model_engine runtimeenv
#include <boost/test/unit_test.hpp>
#include "geo_rasterspace.h"
#include "calc_runtimeenv.h"
#include "calc_spatial.h"

BOOST_AUTO_TEST_CASE(testPopField)
{
  using namespace calc;

  RunTimeEnv rte(geo::RasterSpace(2, 2));
  REAL4 zData[4] = {1, 1, 1, 1};
  auto *sp = new Spatial(VS_S, zData, 4);

  BOOST_TEST(!sp->readOnlyReference());

  sp->setReadOnlyReference(true);
  rte.pushField(sp);

  Field *f = rte.popField();
  BOOST_TEST(f == sp);
  BOOST_TEST(f->readOnlyReference());

  BOOST_TEST(f->vs() == VS_S);
  BOOST_TEST(f->src_f()[0] == 1);
  BOOST_TEST(f->src_f()[3] == 1);

  deleteAlways(sp);
}
