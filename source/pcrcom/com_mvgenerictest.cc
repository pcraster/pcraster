#define BOOST_TEST_MODULE pcraster com mv_generic
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <numeric>
#include "com_mvgeneric.h"


namespace com {
  namespace mvGenericTest {
    struct UP {
     int sum;
     UP():sum(0) {};
     void  operator()(int v) {
       sum+=v;
     }
    };
    struct UPC {
     static int copyCount;
     int sum;
     UPC():sum(0) {
         copyCount++;
     };
     void  operator()(int v) {
       sum+=v;
     }
       UPC(const UPC& rhs) {
         sum=rhs.sum;
         copyCount++;
       }
    };
    int UPC::copyCount =0;

    struct MulSumOp {
     static int copyCount;
     int sum;
     MulSumOp():sum(0) {
         copyCount++;
     };
     void  operator()(INT4 e1, INT4 e2) {
       sum+= (e1*e2);
     }
    };
    int MulSumOp::copyCount =0;

    struct SetSum {
     void  operator()(INT4& e1,const INT4 e2) const
     { e1+=e2; }
     /*
     void  operator()(UINT1& e1,const INT4 e2) const
     { e1+=e2; }
     */
    };


  } // mvGenericTest
} // com


BOOST_AUTO_TEST_CASE(visit_non_mv)
{
  using namespace com;

  {
    UINT1 d[]={ 1, 4, MV_UINT1};
    mvGenericTest::UP up;
    up=forEachNonMV(d,d+3,up);
    BOOST_CHECK(up.sum==5);
    BOOST_CHECK(std::accumulate(d,d+3,0)==260);
    BOOST_CHECK((std::accumulate<UINT1 *,UINT1>(d,d+3,0))==4);
  }
  {
    UINT1 d[]={ 1, 4, MV_UINT1, 1, 4 , 1, 4};
    mvGenericTest::UPC up;
    up=forEachNonMV(d,d+7,up);
    // expect copyCount of 3:
    //  1) default ctor up
    //  2) pass by value
    //  3) return by value
    BOOST_CHECK(mvGenericTest::UPC::copyCount==3);
    BOOST_CHECK(up.sum==15);
  }
}


BOOST_AUTO_TEST_CASE(iterator)
{
  using namespace com;

  typedef NonSpatialContainer<INT4> NSC;
  // typedef NonSpatialIterator<INT4> NSI;
  NSC  c(12,5);

  size_t n=0;
  for(NSC::const_iterator i=c.begin(); i!= c.end(); ++i) {
    BOOST_CHECK(c[n]==12);
    n++;
    BOOST_CHECK(*i==12);
  }
  BOOST_CHECK(n==5);

  mvGenericTest::UPC up;
  up=forEachNonMV(c.begin(),c.end(),up);
  BOOST_CHECK(mvGenericTest::UPC::copyCount==6);
  BOOST_CHECK(up.sum==60);

  INT4 cs[5]={ 1, 0, MV_INT4, 1, 0 };

  typedef mvGenericTest::MulSumOp MSO;
  mvGenericTest::MulSumOp mso;
  mso =forEachNonMV(cs,c,5,MSO());

  BOOST_CHECK(mso.sum==24);

  mso =forEachNonMV(cs,cs,5,MSO());
  BOOST_CHECK(mso.sum==2);

  mso =forEachNonMV(c ,cs,5,MSO());
  BOOST_CHECK(mso.sum==24);

  mso =forEachNonMV(c ,c,5,MSO());
  BOOST_CHECK(mso.sum==(5*12*12));
}


BOOST_AUTO_TEST_CASE(spatial_non_spatial_iterate)
{
  using namespace com;

 {
  UINT1  c[1] ={12};
  INT4 cs[5]={ 1, 0, MV_INT4, 1, 0 };

  typedef mvGenericTest::MulSumOp MSO;
  mvGenericTest::MulSumOp mso;
  mso =iterateNonMV2(cs,5,c,1,mso);
  BOOST_CHECK(mso.sum==24);

  mso =iterateNonMV2(cs,5,cs,5,MSO());
  BOOST_CHECK(mso.sum==2);

  mso =iterateNonMV2(c,1,cs,5,MSO());
  BOOST_CHECK(mso.sum==24);

  mso =iterateNonMV2(c,1,c,1,MSO());
  BOOST_CHECK(mso.sum==(12*12));
 }
 {
  UINT1  c[1] ={12};
  UINT1 cs[5]={ 1, 0, MV_UINT1, 1, 0 };

  typedef mvGenericTest::MulSumOp MSO;
  mvGenericTest::MulSumOp mso;
  mso =iterateNonMV2(cs,5,c,1,mso);
  BOOST_CHECK(mso.sum==24);

  mso =iterateNonMV2(cs,5,cs,5,MSO());
  BOOST_CHECK(mso.sum==2);

  mso =iterateNonMV2(c,1,cs,5,MSO());
  BOOST_CHECK(mso.sum==24);

  mso =iterateNonMV2(c,1,c,1,MSO());
  BOOST_CHECK(mso.sum==(12*12));
 }
}


BOOST_AUTO_TEST_CASE(spatial_non_spatial_for_each)
{
  using namespace com;

 typedef mvGenericTest::SetSum SS;
 {
  UINT1  c[1] ={12};
  INT4 cs[5] ={ 1, 0, MV_INT4, 1, 0 };
  INT4 res[5]={13,12, MV_INT4,13,12 };

  SS ss;
  ss=forEachNonMV2(cs,5,c,1,ss);
  BOOST_CHECK(std::equal(cs,cs+5,res));
 }
}
