#define BOOST_TEST_MODULE pcraster fieldapi readonly_non_spatial
#include <boost/test/unit_test.hpp>
#include "csftypes.h"
#include "fieldapi_readonlynonspatial.h"

template <typename T> void testType(T initVal)
{
  using namespace fieldapi;

  ReadOnlyNonSpatial<T> const t(initVal, 2, 3);
  T v;
  BOOST_TEST(!t.spatial());
  BOOST_TEST(t.nrRows() == 2);
  BOOST_TEST(t.nrCols() == 3);
  BOOST_TEST(t.nrCols() == 3);
  BOOST_TEST(t.get(v, 0, 0));
  BOOST_TEST(v == initVal);
  BOOST_TEST(!t.get(v, 2, 3));
  BOOST_TEST(!t.get(v, -1, 0));
  BOOST_TEST(t.value(0, 0) == initVal);
}

BOOST_AUTO_TEST_CASE(all)
{
  testType<UINT1>(34);
  testType<INT4>(-4);
  testType<INT4>(1058);
  testType<REAL8>(0);
  testType<REAL8>(3.45);
}
