#define BOOST_TEST_MODULE pcraster fieldapi read_write_data
#include <boost/test/unit_test.hpp>
#include "pcrtypes.h"
#include "fieldapi_readwritedata.h"
#include "fieldapi_testfield.h"


template<class UseAsT,class StoredAsT>
void testType()
{
  using namespace fieldapi;

   StoredAsT  d1[ ] = { 0, 1, 2 , 3, 40, 5 };
   pcr::setMV(d1[4]);
   TestField<StoredAsT,2,3> d2(d1);
   ReadWriteData<UseAsT,StoredAsT> t(TEST_FIELD_INIT(d2));

   BOOST_CHECK(t.nrRows()==2);
   BOOST_CHECK(t.nrCols()==3);

   UseAsT v;

   // out of range
   BOOST_CHECK(!t.get(v, 2,3));
   BOOST_CHECK(!t.get(v,-1,0));


   // get value
   BOOST_CHECK(t.get(v,0,0)); BOOST_CHECK(v==0);
   BOOST_CHECK(t.get(v,0,1)); BOOST_CHECK(v==1);
   BOOST_CHECK(t.get(v,0,2)); BOOST_CHECK(v==2);
   BOOST_CHECK(t.get(v,1,0)); BOOST_CHECK(v==3);
   BOOST_CHECK(t.get(v,1,2)); BOOST_CHECK(v==5);

   // get MV
   BOOST_CHECK(!t.get(v,1,1));

   // value
   BOOST_CHECK(t.value(0,0)==0);
   BOOST_CHECK(t.value(0,1)==1);
   BOOST_CHECK(t.value(0,2)==2);
   BOOST_CHECK(t.value(1,0)==3);
   BOOST_CHECK(t.value(1,2)==5);

   for (size_t r=0; r < t.nrRows(); r++)
    for (size_t c=0; c < t.nrCols(); c++) {
      if (t.get(v,r,c)) {
        t.put(v*10,r,c);
        BOOST_CHECK(t.value(r,c) == v*10);

        t.putMV(r,c);
        BOOST_CHECK(!t.get(v,r,c));

        t.put(v,r,c);
        BOOST_CHECK(t.value(r,c) == v);
      }
    }

   t.putAllMV();
   for (size_t r=0; r < t.nrRows(); r++)
    for (size_t c=0; c < t.nrCols(); c++)
        BOOST_CHECK(!t.get(v,r,c));
}


BOOST_AUTO_TEST_CASE(all)
{
 testType<UINT1,UINT1>();
 testType<INT4, UINT1>();
  testType<INT4, INT4>();
 testType<REAL8,UINT1>();
  testType<REAL8,INT4>();
 testType<REAL8,REAL4>();
}
