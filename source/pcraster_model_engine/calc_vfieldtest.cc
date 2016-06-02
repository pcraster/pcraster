#define BOOST_TEST_MODULE pcraster model_engine vfield
#include <boost/test/unit_test.hpp>
#include "calc_vfield.h"
#include "calc_nonspatial.h"
#include "calc_spatial.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

  {
   int ns=3;
   VField<int> vf(ns,5);
   BOOST_CHECK(!vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==3);
   BOOST_CHECK(vf[2]==3);
   BOOST_CHECK(vf[4]==3);
  }
  {
   int s[5]={10,11,12,13,14};
   VField<int> vf(s,5);
   BOOST_CHECK(vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==10);
   BOOST_CHECK(vf[2]==12);
   BOOST_CHECK(vf[4]==14);
  }
  {
   Field *f(new NonSpatial(VS_N,3));
   VField<INT4> vf(f,5);
   BOOST_CHECK(!vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==3);
   BOOST_CHECK(vf[2]==3);
   BOOST_CHECK(vf[4]==3);
  }
  {
   INT4 data[5]={10,11,12,13,14};
   Field *f(new Spatial(VS_N,data,5));

   VField<INT4> vf(f,5);
   BOOST_CHECK(vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==10);
   BOOST_CHECK(vf[2]==12);
   BOOST_CHECK(vf[4]==14);
  }
}

BOOST_AUTO_TEST_CASE(testUpdateMV)
{
  using namespace calc;

  BitField bf(5);
  BOOST_CHECK(bf.none());

  {
   INT4 s[5]={10,11,MV_INT4,13,14};
   VField<INT4> vf(s,5);
   vf.updateMVField(bf);
   BOOST_CHECK(bf.count()==1);
   BOOST_CHECK(bf[2]==1);

  }

  {
   UINT1 s[5]={10,11,13,14,MV_UINT1};
   VField<UINT1> vf(s,5);
   vf.updateMVField(bf);
   BOOST_CHECK(bf.count()==2);
   BOOST_CHECK(bf[2]==1);
   BOOST_CHECK(bf[4]==1);
  }

  {
   Field *f(new NonSpatial(VS_N,3));
   VField<INT4> vf(f,5);
   BOOST_CHECK(bf.count()==2);
   BOOST_CHECK(bf[2]==1);
   BOOST_CHECK(bf[4]==1);
  }
}
