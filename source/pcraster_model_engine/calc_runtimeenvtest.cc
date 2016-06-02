#define BOOST_TEST_MODULE pcraster model_engine runtimeenv
#include <boost/test/unit_test.hpp>
#include "geo_rasterspace.h"
#include "calc_runtimeenv.h"
#include "calc_spatial.h"




BOOST_AUTO_TEST_CASE(testPopField)
{
  using namespace calc;

  RunTimeEnv      rte(geo::RasterSpace(2,2));
  REAL4 zData[4]= { 1,1,1,1};
  Spatial *sp=new Spatial(VS_S,zData,4);

  BOOST_CHECK(!sp->readOnlyReference());

  sp->setReadOnlyReference(true);
  rte.pushField(sp);

  Field *f= rte.popField();
  BOOST_CHECK(f==sp);
  BOOST_CHECK(f->readOnlyReference());

  BOOST_CHECK(f->vs() == VS_S);
  BOOST_CHECK(f->src_f()[0] == 1);
  BOOST_CHECK(f->src_f()[3] == 1);

  deleteAlways(sp);
}
