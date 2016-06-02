#define BOOST_TEST_MODULE pcraster model_engine fopointarray
#include <boost/test/unit_test.hpp>
#include "com_math.h"
#include "calc_foarrayimplmanual.h"
#include "calc_fopointarray.h"
#include "calc_fopointimpl.h"


BOOST_AUTO_TEST_CASE(test1)
{
  using namespace calc;

#include "calc_fopointarrayimpl.inc"

{
 INT4  l[3]={1,2,3};
 float  r[3];
 pcr::setMV(l[1]);
 foAr_c_4_2_s_4.f(r, l,3);
 BOOST_CHECK(r[0]==1);
 BOOST_CHECK(pcr::isMV(r[1]));
 BOOST_CHECK(r[2]==3);
}
 {
  float l[4] = { 10, 4, 8, 0 };
  float r[4] = { 4, 10, 0, 9 };
  pcr::setMV(l[3]);
  pcr::setMV(r[2]);
  UINT1 res[4];
  foAr_gt_f.ss(res,l,r,4);
  BOOST_CHECK(res[0]==1);
  BOOST_CHECK(res[1]==0);
  BOOST_CHECK(res[2]==MV_UINT1);
  BOOST_CHECK(res[3]==MV_UINT1);
 }
{
  float l[4] = { 10, 4, 8, 0 };
  float r[1] = { 4 };
  pcr::setMV(l[3]);
  UINT1 res[4];
  foAr_gt_f.sn(res,l,r,4);
  BOOST_CHECK(res[0]==1);
  BOOST_CHECK(res[1]==0);
  BOOST_CHECK(res[2]==1);
  BOOST_CHECK(res[3]==MV_UINT1);
 }
{
  float l[1] = { 4 };
  float r[4] = { 10, 4, 2, 0 };
  pcr::setMV(r[3]);
  UINT1 res[4];
  foAr_gt_f.ns(res,l,r,4);
  BOOST_CHECK(res[0]==0);
  BOOST_CHECK(res[1]==0);
  BOOST_CHECK(res[2]==1);
  BOOST_CHECK(res[3]==MV_UINT1);
 }
{
 {
  float l[2] = { 10, 4 };
  float r[2] = { 4,  0 };
  foAr_mod_f.ss(l,r,2);
  BOOST_CHECK(l[0]==2);
  BOOST_CHECK(pcr::isMV(l[1]));
  BOOST_CHECK(r[0]==4);
  BOOST_CHECK(r[1]==0);
 }
 {
  float l[1] = { 10 };
  float r[2] = { 4,  0 };
  foAr_mod_f.ns(l,r,2);
  BOOST_CHECK(l[0]==10);
  BOOST_CHECK(r[0]==2);
  BOOST_CHECK(pcr::isMV(r[1]));
 }
 {
  float l[2] = { 10, 4 };
  float r[1] = {  0 };
  bool catched(false);
  try {
    foAr_mod_f.sn(l,r,2);
  } catch(const DomainError& ) {
    catched=true;
  }
  BOOST_CHECK(catched);
 }
}
{
 {
  float l[3]={-1,2,4};
  float r[3]={-1,2,4};
  pcr::setMV(r[1]);
  foAr_badd_f.ss( l,r,3);
  BOOST_CHECK(l[0]==-2);
  BOOST_CHECK(r[0]==-1);
  BOOST_CHECK(pcr::isMV(l[1]));
  BOOST_CHECK(pcr::isMV(r[1]));
  BOOST_CHECK(l[2]==8);
  BOOST_CHECK(r[2]==4);
 }
 {
  float l[1]={10};
  float r[3]={1,2,4};
  pcr::setMV(r[1]);
  foAr_badd_f.ns( l,r,3);
  BOOST_CHECK(l[0]==10);
  BOOST_CHECK(r[0]==11);
  BOOST_CHECK(pcr::isMV(r[1]));
  BOOST_CHECK(r[2]==14);
 }
 {
  float l[1]={10};
  float r[3]={1,2,4};
  pcr::setMV(r[1]);
  foAr_badd_f.sn(r,l,3);  // swap as above
  BOOST_CHECK(l[0]==10);
  BOOST_CHECK(r[0]==11);
  BOOST_CHECK(pcr::isMV(r[1]));
  BOOST_CHECK(r[2]==14);
 }
}
{
 float l[3]={-1,2,4};
 pcr::setMV(l[1]);
 foAr_sqrt_f.f( l,3);
 BOOST_CHECK(pcr::isMV(l[1]));
 BOOST_CHECK(l[2]==2);
 BOOST_CHECK(pcr::isMV(l[0]));
}
{
 float l[3]={1,2,3};
 foAr_umin_f.f( l,3);
 BOOST_CHECK(l[0]==-1);
 BOOST_CHECK(l[1]==-2);
 BOOST_CHECK(l[2]==-3);
}
{
 INT4 l[3]={5,0,MV_INT4};
 UINT1 r[3];
 foAr_c_4_2_b_4.f(r, l,3);
 BOOST_CHECK(r[0]==1);
 BOOST_CHECK(r[1]==0);
 BOOST_CHECK(r[2]==MV_UINT1);
 BOOST_CHECK(l[0]==5);
 BOOST_CHECK(l[1]==0);
 BOOST_CHECK(l[2]==MV_INT4);
}
{ // pcrcalc337a
    REAL4 r= -0.047F;
    REAL4 l= 1.5F;
    BOOST_CHECK(special::pow_domainIll(r,l));
}
}

BOOST_AUTO_TEST_CASE(testAggregate)
{
  using namespace calc;

 {
  UINT1 r,i[4]={MV_UINT1,4,0,3};
  AggregateArray< MapTotal<UINT1> > f;
  f.fImpl(&r,i,4);
  BOOST_CHECK(r==7);
 }
 {
  INT4 r,i[4]={MV_INT4,4,0,3};
  AggregateArray< MapMaximum<INT4> > f;
  f.fImpl(&r,i,4);
  BOOST_CHECK(r == 4);
 }
}
