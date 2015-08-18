#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_COM_CSFCELLTEST
#include "com_csfcelltest.h"
#define INCLUDED_COM_CSFCELLTEST
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::CSFCellTest::suite()
{
  /*
   * also tests aspects of pcrtypes.h
   */
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CSFCellTest> instance(new CSFCellTest());
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testCsfSizes, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testCastAndCopyCells, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testSetMV, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testAlterToStdMV, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testGetMinMax, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testFromStdMV, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testEndianSwap, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testCsfCellMax, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testIsType, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFCellTest::testLessMV, instance));
  return suite;
}


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::CSFCellTest::CSFCellTest()
{
}

void com::CSFCellTest::setUp()
{
}

void com::CSFCellTest::tearDown()
{
}

void com::CSFCellTest::testCsfSizes()
{
  BOOST_CHECK(sizeof(UINT1) == 1);
  BOOST_CHECK(sizeof( INT1) == 1);
  BOOST_CHECK(sizeof(UINT2) == 2);
  BOOST_CHECK(sizeof( INT2) == 2);
  BOOST_CHECK(sizeof(UINT4) == 4);
  BOOST_CHECK(sizeof( INT4) == 4);
  BOOST_CHECK(sizeof(REAL4) == 4);
  BOOST_CHECK(sizeof(REAL8) == 8);
}

//! does also CastCell, since copy is no more than a loop around CastCell
void com::CSFCellTest::testCastAndCopyCells()
{
  #define NR 10
  INT4  *int4 = new INT4[NR];

  UINT1 *uint1  = reinterpret_cast<UINT1 *>(int4);
  for(size_t i=0; i < 10; i++)
   int4[i]=i;
  copyCells(uint1,int4,NR);
  for(size_t i=0; i < 10; i++)
   BOOST_CHECK(static_cast<size_t>(uint1[i])==i);

  copyCells(int4,uint1,NR);
  for(size_t i=0; i < 10; i++)
   BOOST_CHECK(int4[i]== static_cast<INT4>(i));

  delete [] int4;

  {
  // FAILED when pcr::isMV(INT2 ) is not implemented
  // because type promotion will use pcr::isMV(INT4) where
  //  pcr::isMV(INT2) is needed
  INT2 i2=MV_INT2;
  INT4 i4;
  copyCells(&i4,&i2,1);
  BOOST_CHECK(i4==MV_INT4);
  }

  {
  INT4 i4=MV_INT4;
  INT2 i2;
  copyCells(&i2,&i4,1);
  BOOST_CHECK(i2==MV_INT2);
  }
  {
  double d=14;
  UINT1 i1;
  CastCell<UINT1,double>()(i1,d);
  BOOST_CHECK(i1==14);
  }
  {
  double d;
  pcr::setMV(d);
  UINT1 i1;
  CastCell<UINT1,double>()(i1,d);
  BOOST_CHECK(i1==MV_UINT1);
  }
  {
 /*
  *float d;
  *SetMV()(d);
  *UINT1 i1;
  *CastCell<UINT1,float>()(i1,d);
  *BOOST_CHECK(i1==MV_UINT1);
  */
  }
}

void com::CSFCellTest::testSetMV()
{
  // a generic
  INT4 int4[10];
  std::memset(int4,0,10*sizeof(INT4));
  pcr::setMV(int4,10);
  for(size_t i=0; i < 10; i++) {
   BOOST_CHECK(pcr::isMV(int4[i]));
   BOOST_CHECK(pcr::isMV(int4+i));
  }

  // the specialization UINT1
  UINT1 uint1[10];
  std::memset(uint1,0,10*sizeof(UINT1));
  pcr::setMV(uint1,10);
  for(size_t i=0; i < 10; i++) {
   BOOST_CHECK(pcr::isMV(uint1[i]));
   BOOST_CHECK(pcr::isMV(uint1+i));
  }

  // the specialization REAL4
  REAL4 real4[10];
  std::memset(real4,0,10*sizeof(REAL4));
  pcr::setMV(real4,10);
  for(size_t i=0; i < 10; i++) {
   BOOST_CHECK(pcr::isMV(real4[i]));
   BOOST_CHECK(pcr::isMV(real4+i));
  }
  {
   double real8;
   pcr::setMV(real8);
   UINT1 *c=(UINT1 *)&real8;
   BOOST_CHECK(c[0]==MV_UINT1);
   BOOST_CHECK(c[1]==MV_UINT1);
   BOOST_CHECK(c[2]==MV_UINT1);
   BOOST_CHECK(c[3]==MV_UINT1);
   BOOST_CHECK(c[4]==MV_UINT1);
   BOOST_CHECK(c[5]==MV_UINT1);
   BOOST_CHECK(c[6]==MV_UINT1);
   BOOST_CHECK(c[7]==MV_UINT1);
  }
}

void com::CSFCellTest::testGetMinMax()
{
{
  UINT1 v[3]={0,3,MV_UINT1};
  GetMinMax<UINT1> g(MV_UINT1);
  g.add(v,3);
  BOOST_CHECK(g.min()==0);
  BOOST_CHECK(g.max()==3);
}
{ // no add
  UINT1 v[3]={0,3,MV_UINT1};
  GetMinMax<UINT1> g(MV_UINT1);
  BOOST_CHECK(g.min()==MV_UINT1);
  BOOST_CHECK(g.max()==MV_UINT1);
  g.add(v,0);
  BOOST_CHECK(g.min()==MV_UINT1);
  BOOST_CHECK(g.max()==MV_UINT1);
}
{ // add 1
  UINT1 v[1]={0};
  GetMinMax<UINT1> g(MV_UINT1);
  g.add(v,1);
  BOOST_CHECK(g.min()==0);
  BOOST_CHECK(g.max()==0);
}
{ // add 1 MV
  UINT1 v[1]={MV_UINT1};
  GetMinMax<UINT1> g(MV_UINT1);
  g.add(v,1);
  BOOST_CHECK(g.min()==MV_UINT1);
  BOOST_CHECK(g.max()==MV_UINT1);
}
{ // add 2 MV
  UINT1 v[3]={MV_UINT1,MV_UINT1,MV_UINT1};
  GetMinMax<UINT1> g(MV_UINT1);
  g.add(v,3);
  BOOST_CHECK(g.min()==MV_UINT1);
  BOOST_CHECK(g.max()==MV_UINT1);
}
{ // float -999
  REAL4 v[3]={0,3,-999};
  GetMinMax<REAL4> g(-999);
  g.add(v,3);
  BOOST_CHECK(g.min()==0);
  BOOST_CHECK(g.max()==3);
}
}

void com::CSFCellTest::testAlterToStdMV()
{
  {
    UINT1 v[3]={0,3,MV_UINT1};
    pcr::AlterToStdMV<UINT1> t(3);
    std::for_each(v,v+3,t);
    BOOST_CHECK(v[0]==0);
    BOOST_CHECK(v[1]==MV_UINT1);
    BOOST_CHECK(v[2]==MV_UINT1);
  }
  {
    REAL4 v[3]={0,3,-8};
    pcr::setMV(v[2]);
    std::for_each(v,v+3,pcr::AlterToStdMV<REAL4>(3));
    BOOST_CHECK(v[0]==0);
    BOOST_CHECK(pcr::isMV(v[1]));
    BOOST_CHECK(pcr::isMV(v[2]));
  }
}

void com::CSFCellTest::testFromStdMV()
{
  {
    UINT1 v[3]={0,3,MV_UINT1};
    pcr::FromStdMV<UINT1> t(3);
    std::transform(v,v+3,v,t);
    BOOST_CHECK(v[0]==0);
    BOOST_CHECK(v[1]==3);
    BOOST_CHECK(v[2]==3);
  }
  {
    REAL4 v[3]={0,3,-1};
    pcr::setMV(v[2]);
    std::transform(v,v+3,v,pcr::FromStdMV<REAL4>(-999));
    BOOST_CHECK(v[0]==0);
    BOOST_CHECK(v[1]==3);
    BOOST_CHECK(!pcr::isMV(v[2]));
    BOOST_CHECK(v[2]==-999);
  }
}

void com::CSFCellTest::testCsfCellMax()
{
    REAL4 v[3]={0,3,-1};
    pcr::setMV(v[0]);
    BOOST_CHECK(pcr::isMV(v[0]));
    const REAL4 *max=csfCellMax(v,v+3);
    BOOST_CHECK(max==v+1);
    BOOST_CHECK(*max==3);

    // all MV
    pcr::setMV(v[1]);
    BOOST_CHECK(pcr::isMV(v[0]));
    BOOST_CHECK(pcr::isMV(v[1]));
    max=csfCellMax(v,v+2);
    BOOST_CHECK(max==v+2);

    // all MV
    max=csfCellMax(v,v);
    BOOST_CHECK(max==v);
}

void com::CSFCellTest::testEndianSwap()
{
  {
    INT2 v[3]={0,3,MV_INT2};
    std::for_each(v,v+3,EndianSwapINT2());
    BOOST_CHECK(v[0]==0); // 0 is "symmetric"
    BOOST_CHECK(v[1]!=3);
    BOOST_CHECK(v[2]!=MV_INT2);
  }
  {
    REAL4 v[2]={0,-1};
    pcr::setMV(v[1]);
    std::for_each(v,v+2,EndianSwapREAL4());
    BOOST_CHECK(v[0]==0);// 0 is "symmetric"
    BOOST_CHECK(pcr::isMV(v[1]));// MV_UINT4,MV_REAL4 is "symmetric"
  }
}

void com::CSFCellTest::testIsType()
{
  BOOST_CHECK(isUINT1(0));
  BOOST_CHECK(isUINT1(UINT1_MIN));
  BOOST_CHECK(isUINT1(UINT1_MAX));
  BOOST_CHECK(isUINT1(8));

  BOOST_CHECK(!isUINT1(MV_UINT1));
  BOOST_CHECK(!isUINT1(-2));
  BOOST_CHECK(!isUINT1(7.34));
  BOOST_CHECK(!isUINT1(7.0000007));

  BOOST_CHECK(isINT2(0));
  BOOST_CHECK(isINT2(INT2_MIN));
  BOOST_CHECK(isINT2(INT2_MAX));
  BOOST_CHECK(isINT2(8));
  BOOST_CHECK(isINT2(-2));

  BOOST_CHECK(!isINT2(MV_INT2));
  BOOST_CHECK(!isINT2(-7.32));
  BOOST_CHECK(!isINT2(7.0000007));

}

void com::CSFCellTest::testLessMV()
{
 {
  UINT1  u1(1);
  UINT1  u5(5);
  BOOST_CHECK( lessMV(u1,u5));
  BOOST_CHECK(!lessMV(u5,u1));
  BOOST_CHECK(!lessMV(u1,u1));
  BOOST_CHECK( lessMV(u1,MV_UINT1));
  BOOST_CHECK(!lessMV(MV_UINT1,u1));
 }
 {
  INT4  u1(1);
  INT4  u5(5);
  BOOST_CHECK( lessMV(u1,u5));
  BOOST_CHECK(!lessMV(u5,u1));
  BOOST_CHECK(!lessMV(u1,u1));
  BOOST_CHECK( lessMV(u1,MV_INT4));
  BOOST_CHECK(!lessMV(MV_INT4,u1));
 }
}
