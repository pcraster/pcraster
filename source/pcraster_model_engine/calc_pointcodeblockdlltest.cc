#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POINTCODEBLOCKDLLTEST
#include "calc_pointcodeblockdlltest.h"
#define INCLUDED_CALC_POINTCODEBLOCKDLLTEST
#endif

// Library headers.
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

// PCRaster library headers.
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif

// Module headers.
#ifndef INCLUDED_CALC_P5STACK
#include "calc_p5stack.h"
#define INCLUDED_CALC_P5STACK
#endif
#ifndef INCLUDED_CALC_POINTCODEDLLHEADER
#include "calc_pointcodedllheader.h"
#define INCLUDED_CALC_POINTCODEDLLHEADER
#endif



/*!
  \file
  This file contains the implementation of the PointCodeBlockDllTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTCODEBLOCKDLL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::PointCodeBlockDllTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PointCodeBlockDllTest> instance(new PointCodeBlockDllTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PointCodeBlockDllTest::test_f, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PointCodeBlockDllTest::testIfThenElse, instance));
// DOES NOT YET COMPILE WITH Scons
//  suite->add(BOOST_CLASS_TEST_CASE(&PointCodeBlockDllTest::testCompile, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF POINTCODEBLOCKDLL MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::PointCodeBlockDllTest::PointCodeBlockDllTest()
{
}



//! setUp
void calc::PointCodeBlockDllTest::setUp()
{
}



//! tearDown
void calc::PointCodeBlockDllTest::tearDown()
{
}

void calc::PointCodeBlockDllTest::testIfThenElse()
{
 // _ifthenelse(A &result, const UINT1& arg0, const A& arg1,const A& arg2)
 {
   REAL4 r=-999;
   _ifthenelse<>(r,MV_UINT1,(REAL4)1.0,(REAL4)2.0);
   BOOST_CHECK(pcr::isMV(r));
 }
 {
   REAL4 r=-999;
   _ifthenelse<>(r,0,(REAL4)1.0,(REAL4)2.0);
   BOOST_CHECK(r==2);
 }
 {
   REAL4 r=-999, arg2;
   pcr::setMV(arg2);
   _ifthenelse<>(r,0,(REAL4)1.0,arg2);
   BOOST_CHECK(pcr::isMV(r));
 }
 {
   REAL4 r=-999;
   _ifthenelse<>(r,1,(REAL4)1.0,(REAL4)2.0);
   BOOST_CHECK(r==1);
 }
 {
   REAL4 r=-999, arg1;
   pcr::setMV(arg1);
   _ifthenelse<>(r,1,arg1,(REAL4)2);
   BOOST_CHECK(pcr::isMV(r));
 }
}

void calc::PointCodeBlockDllTest::test_f()
{
 {
   // SameUn
   BOOST_CHECK(_f<point::sqrt<REAL4 > >(4.0) == 2.0);
 }
 {
   // SameBin
   BOOST_CHECK(_f<point::badd<REAL4> >(4.0,4.0) == 8.0);
 }

 {  // DiffUn
   bool t1(_f<point::nodirection<UINT1,REAL4 > >(-1) == 1);
   BOOST_CHECK(t1);
   bool t2(_f<point::nodirection<UINT1,REAL4 > >( 1) == 0);
   BOOST_CHECK(t2);
 }
 {  // DiffBin
   BOOST_CHECK(_f<point::eq<REAL4 > >(-1,-1) == 1);
   BOOST_CHECK(_f<point::ne<REAL4 > >(-1,-1) == 0);
 }
}

void calc::PointCodeBlockDllTest::testCompile()
{
  struct P5StackC : public P5Stack {
    P5StackC(const char *code):
      P5Stack(CompileTest(code))
    {
    }
  };

#ifdef WIN32
   bool addWin32DllCompile=false;
   BOOST_WARN(addWin32DllCompile);
#else

  {
   geo::FileCreateTester fct("tmp.res");
   P5StackC("tmp = inp1s.map*0+5-inp5s.map+1*1; report tmp.res=tmp");
   BOOST_CHECK(fct.equalTo("inp1s.map",false));
  }
  {
   geo::FileCreateTester fct("tmp.res");
   P5StackC(
       "tmp = 5-inp5s.map + 0;"\
       "tmp = 1*if(inp1s.map gt 4,inp1s.map*0,tmp+1*1);"\
       "tmp = tmp * 1 + 0;"\
       "report tmp.res=tmp");
   BOOST_CHECK(fct.equalTo("inp1s.map",false));
  }
#endif
  /*! 1) test stack order
   *  a= inp1s.map*1;     # a == inp1s.map
   *  b= inp1s.map*0+5;   # b == inp5s.map
   *  c= b*0;             # c == inp0s.map
   */

  // 2) test with update (some input is output)
  // 3) test with some/only non-spatials
  // 4) test that fails if non-spatial input/output are accumalted
  //         repeat until
}
