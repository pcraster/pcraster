#define BOOST_TEST_MODULE pcraster model_engine vfield
#include <boost/test/unit_test.hpp>
#include "calc_vfield.h"
#include "calc_nonspatial.h"
#include "calc_spatial.h"

BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

  {
    int const ns = 3;
    VField<int> const vf(ns, 5);
    BOOST_TEST(!vf.spatial());
    BOOST_TEST(vf.size() == 5);
    BOOST_TEST(vf[0] == 3);
    BOOST_TEST(vf[2] == 3);
    BOOST_TEST(vf[4] == 3);
  }
  {
    int s[5] = {10, 11, 12, 13, 14};
    VField<int> const vf(s, 5);
    BOOST_TEST(vf.spatial());
    BOOST_TEST(vf.size() == 5);
    BOOST_TEST(vf[0] == 10);
    BOOST_TEST(vf[2] == 12);
    BOOST_TEST(vf[4] == 14);
  }
  {
    Field *f(new NonSpatial(VS_N, 3));
    VField<INT4> const vf(f, 5);
    BOOST_TEST(!vf.spatial());
    BOOST_TEST(vf.size() == 5);
    BOOST_TEST(vf[0] == 3);
    BOOST_TEST(vf[2] == 3);
    BOOST_TEST(vf[4] == 3);
  }
  {
    INT4 data[5] = {10, 11, 12, 13, 14};
    Field *f(new Spatial(VS_N, data, 5));

    VField<INT4> const vf(f, 5);
    BOOST_TEST(vf.spatial());
    BOOST_TEST(vf.size() == 5);
    BOOST_TEST(vf[0] == 10);
    BOOST_TEST(vf[2] == 12);
    BOOST_TEST(vf[4] == 14);
  }
}

BOOST_AUTO_TEST_CASE(testUpdateMV)
{
  using namespace calc;

  BitField bf(5);
  BOOST_TEST(bf.none());

  {
    INT4 s[5] = {10, 11, MV_INT4, 13, 14};
    VField<INT4> const vf(s, 5);
    vf.updateMVField(bf);
    BOOST_TEST(bf.count() == 1);
    BOOST_TEST(bf[2] == 1);
  }

  {
    UINT1 s[5] = {10, 11, 13, 14, MV_UINT1};
    VField<UINT1> const vf(s, 5);
    vf.updateMVField(bf);
    BOOST_TEST(bf.count() == 2);
    BOOST_TEST(bf[2] == 1);
    BOOST_TEST(bf[4] == 1);
  }

  {
    Field *f(new NonSpatial(VS_N, 3));
    VField<INT4> const vf(f, 5);
    BOOST_TEST(bf.count() == 2);
    BOOST_TEST(bf[2] == 1);
    BOOST_TEST(bf[4] == 1);
  }
}
