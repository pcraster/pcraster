#define BOOST_TEST_MODULE pcraster com mv_op
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <bitset>
#include "com_mvop.h"
#include "com_csfcell.h"


#ifndef PCRASTER_LSB
template<typename T>
static std::string asStr(char const* bits)
{
   std::string res;

   UINT4 *v=(UINT4 *)bits;
   std::bitset<32> bs(v[0]);
   res = bs.to_string();
   if (sizeof(T) == 8) {
     bs = std::bitset<32>(v[1]);
     res += bs.to_string();
   }
   return res;
}
#endif


template<typename T>
void testOp()
{
  using namespace com;

  typedef MVOp<T> R;

  {
   R r=(T)1;
   T f=2;
   r += f;
   BOOST_CHECK(r==3);
   BOOST_CHECK(r+5==8);
   BOOST_CHECK(5+r==8);
  }
  {
   T f=1;
   R r;
   r.setMV();
   BOOST_CHECK(r.isMV());
   r+=f;
   BOOST_CHECK(r.isMV());
   // assignment to T must propagate MV
   f=r();
   BOOST_CHECK(pcr::isMV(f));

   // initializing R with MV T
   R fr(f);
   BOOST_CHECK(fr.isMV());

   // BOOST_CHECK_EQUAL(asStr<T>(fr.debugBits()), std::string("11111111"));
   // BOOST_CHECK_EQUAL(asStr<T>(R(f).debugBits()),std::string("11111111"));

   // initializing R with MV T
   BOOST_CHECK(R(f).isMV());
   BOOST_CHECK(pcr::isMV( R(f)() ));

  }
  {
    T f1; // __attribute__((aligned(8)));
    T f2; // __attribute__((aligned(8)));
    pcr::setMV(f1);
    f2=3;


    BOOST_CHECK(pcr::isMV((R(f1)+f2)()) );
    BOOST_CHECK(pcr::isMV((R(f1)-f2)()) );
  }
  {
   T f=1;
   R r;
   r.setMV();
   r+=f;
   BOOST_CHECK(r.isMV());
  }

  {
   R r=(T)2;
   T f=1;
   f-=r();
   BOOST_CHECK(f==-1);
  }

  {
   R r=(T)1;
   T f;
   pcr::setMV(f);
   r-=f;
   BOOST_CHECK(r.isMV());
  }
  {
   R r=(T)1;
   T f=1;
   BOOST_CHECK( (r-f)()==0);
  }

  {
   R r=(T)2;
   T f=3;
   r*=f;
   BOOST_CHECK(r()==6);
  }

  {
   T f=3;
   R r;
   r.setMV();
   r*=f;
   BOOST_CHECK(r.isMV());
  }
}


#ifndef PCRASTER_LSB
BOOST_AUTO_TEST_CASE(test_op)
{
  testOp<float>();
  testOp<double>();
}
#endif


BOOST_AUTO_TEST_CASE(static_ass)
{
  using namespace com;

  typedef MVOp<float> R;
  float dest=1;
  float src =3;
  BOOST_CHECK(R::addAss(dest,src)==4);
  BOOST_CHECK(dest==4);

  // dest += 2*3;
  BOOST_CHECK(R::addAss(dest,R(2.0f)*3.0f)==10);
  BOOST_CHECK(dest==10);

}

// to check BOOST_STATIC_ASSERT's compile time
// change from OFF to ON
#define STATIC_ASSERT_ON(statement) statement
#define STATIC_ASSERT_OFF(statement)


BOOST_AUTO_TEST_CASE(static_asserts)
{
  using namespace com;

  struct Test {
  // no object size bloat allowed, only size of single value d_v
    BOOST_STATIC_ASSERT(sizeof(float) ==sizeof(MVOp<float>));
    BOOST_STATIC_ASSERT(sizeof(double)==sizeof(MVOp<double>));
  };
  BOOST_CHECK(sizeof(float)==sizeof(MVOp<float>));
  BOOST_CHECK(sizeof(double)==sizeof(MVOp<double>));

  // only float and double allowed
  // BOOST_STATIC_ASSERT
  // no int
  STATIC_ASSERT_OFF(MVOp<int> i);
  // no long double
  // TODO: but Microsoft compiler has 8 byte double
  STATIC_ASSERT_OFF(MVOp<long double> l);
}


BOOST_AUTO_TEST_CASE(implicit_cast_static_asserts)
{
  using namespace com;

 { // cannot stick float into double
  float w4=3;
  MVOp<double> v8(1.0);
  STATIC_ASSERT_OFF(v8-=w4);
 }
 { // double into float
  double w8= 6;
  MVOp<float> v4(1.0f);
  STATIC_ASSERT_OFF(MVOp<float> v8(w8));
  STATIC_ASSERT_OFF(v4-=w8);
 }
 { // double& into float
  double w8=3;
  double& ref8(w8);
  MVOp<float> v4(1.0f);
  STATIC_ASSERT_OFF(v4-=ref8);
 }
 {
   typedef MVOp<double> D;
   STATIC_ASSERT_OFF(D::addAss(dest,D(3.0f))==4);
 }
}
