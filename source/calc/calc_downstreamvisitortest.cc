#define BOOST_TEST_MODULE pcraster calc downstream_visitor
#include <boost/test/unit_test.hpp>
#include "fieldapi_testfield.h"
#include "fieldapi_interface.h"
#include "calc_downstreamvisitor.h"


BOOST_AUTO_TEST_CASE(realistic_ldd)
{
  using namespace calc;

 {
  UINT1 mask1[]={ MV_UINT1, 2,        2, MV_UINT1, 1,
                  5,        4,        4,        4, MV_UINT1,
                  MV_UINT1, 6,        9, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> mask2(mask1);
  fieldapi::ReadWriteData<UINT1,UINT1> mask(TEST_FIELD_INIT(mask2));
  mask.putAllMV();

  UINT1 d1[]={ MV_UINT1, 2,  2, MV_UINT1, 1,
               5,        4,  4, 4,        MV_UINT1,
               MV_UINT1, 6,  9, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));


  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,0)); v.valid(); ++v) {
     for(geo::UpstreamNeighbourVisitor u(*v); u.valid(); ++u)
        BOOST_CHECK(mask.value(u.nb())==1);

     count++;
     UINT1 m;
     BOOST_CHECK(!mask.get(m,*v));
     mask.put(1,*v);
  }
  BOOST_CHECK(count==9);
 }
}


// ldd's with single upstream neighbour: known order
BOOST_AUTO_TEST_CASE(known_order)
{
  using namespace calc;

 {
  UINT1 d1[]={ 5, 4, 4 };
  fieldapi::TestField<UINT1,1,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(0,0)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(0,2-count));
     count++;
  }
  BOOST_CHECK(count==3);
 }
 {
  UINT1 d1[]={ MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1,  5, 4, 4, MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,1)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(1,3-count));
     count++;
  }
  BOOST_CHECK(count==3);
 }
 {
  UINT1 d1[]={ 6, 6, 5 };
  fieldapi::TestField<UINT1,1,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(0,2)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(0,count));
     count++;
  }
  BOOST_CHECK(count==3);
 }
 {
  UINT1 d1[]={ MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1,  6, 6, 5, MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,3)); v.valid(); ++v) {
     BOOST_CHECK(*v == geo::CellLoc(1,count+1));
     count++;
  }
  BOOST_CHECK(count==3);
 }
}


BOOST_AUTO_TEST_CASE(all_directions)
{
  using namespace calc;

 {
  UINT1 d1[]={ 3, 2, 1,
               6, 5, 4,
               9, 8, 7 };
  fieldapi::TestField<UINT1,3,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,1)); v.valid(); ++v) {
     count++;
  }
  BOOST_CHECK(count==9);
 }
 {
  UINT1 d1[]= {MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1, 3, 2, 1, MV_UINT1,
               MV_UINT1, 6, 5, 4, MV_UINT1,
               MV_UINT1, 9, 8, 7, MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,5,5> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(2,2)); v.valid(); ++v) {
     count++;
  }
  BOOST_CHECK(count==9);
 }
}


BOOST_AUTO_TEST_CASE(pit_only)
{
  using namespace calc;

 {
  UINT1 d1[]={ 5 };
  fieldapi::TestField<UINT1,1,1> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));


  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(0,0)); v.valid(); ++v) {
     count++;
  }
  BOOST_CHECK(count==1);
 }
 {
  UINT1 d1[]={ MV_UINT1, MV_UINT1, MV_UINT1,
               MV_UINT1,   5     , MV_UINT1,
               MV_UINT1, MV_UINT1, MV_UINT1 };
  fieldapi::TestField<UINT1,3,3> d2(d1);
  fieldapi::ReadOnlySpatial<UINT1,UINT1> f(TEST_FIELD_INIT(d2));

  bool shouldNotReach=true;
  size_t count=0;
  for(DownStreamVisitor v(f,geo::CellLoc(1,1)); v.valid(); ++v) {
     count++;
     for(geo::UpstreamNeighbourVisitor u(*v); u.valid(); ++u)
        shouldNotReach=false;
  }
  BOOST_CHECK(shouldNotReach);
  BOOST_CHECK(count==1);
 }
// test with   2*2 -> all MV not possible,yet
/*
 test with   1*1 -> pit
 */
}
