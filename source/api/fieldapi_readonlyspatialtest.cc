#define BOOST_TEST_MODULE pcraster fieldapi readonly_spatial
#include <boost/test/unit_test.hpp>
#include "pcrtypes.h"
#include "fieldapi_readonlyspatial.h"
#include "fieldapi_testfield.h"

template <typename UseAsT, typename StoredAsT> void testType()
{
  using namespace fieldapi;

  StoredAsT d1[] = {0, 1, 2, 3, 40, 5};
  pcr::setMV(d1[4]);
  TestField<StoredAsT, 2, 3> d2(d1);
  ReadOnlySpatial<UseAsT, StoredAsT> const t(TEST_FIELD_INIT(d2));
  UseAsT v;

  BOOST_TEST(t.spatial());
  BOOST_TEST(t.nrRows() == 2);
  BOOST_TEST(t.nrCols() == 3);

  // out of range
  BOOST_TEST(!t.get(v, 2, 3));
  BOOST_TEST(!t.get(v, -1, 0));


  // get value
  BOOST_TEST(t.get(v, 0, 0));
  BOOST_TEST(v == 0);
  BOOST_TEST(t.get(v, 0, 1));
  BOOST_TEST(v == 1);
  BOOST_TEST(t.get(v, 0, 2));
  BOOST_TEST(v == 2);
  BOOST_TEST(t.get(v, 1, 0));
  BOOST_TEST(v == 3);
  BOOST_TEST(t.get(v, 1, 2));
  BOOST_TEST(v == 5);

  // get MV
  BOOST_TEST(!t.get(v, 1, 1));

  // value
  BOOST_TEST(t.value(0, 0) == 0);
  BOOST_TEST(t.value(0, 1) == 1);
  BOOST_TEST(t.value(0, 2) == 2);
  BOOST_TEST(t.value(1, 0) == 3);
  BOOST_TEST(t.value(1, 2) == 5);
}

BOOST_AUTO_TEST_CASE(all)
{
  using namespace fieldapi;

  testType<UINT1, UINT1>();

  testType<INT4, UINT1>();
  testType<INT4, INT4>();

  testType<REAL8, UINT1>();
  testType<REAL8, INT4>();
  testType<REAL8, REAL4>();
}
