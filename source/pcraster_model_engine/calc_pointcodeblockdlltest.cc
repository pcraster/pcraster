#define BOOST_TEST_MODULE pcraster model_engine pointcodeblockdll
#include <boost/test/unit_test.hpp>
#include "geo_filecreatetester.h"
#include "calc_p5stack.h"
#include "calc_pointcodedllheader.h"
#include "calc_globallibdefs.h"



struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()
    {
        calc::globalEnd();
    }

};

BOOST_FIXTURE_TEST_SUITE(pointcodeblockdll, Fixture)

BOOST_AUTO_TEST_CASE(testReportDefault)
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

BOOST_AUTO_TEST_CASE(test_f)
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

BOOST_AUTO_TEST_CASE(testCompile)
{
  using namespace calc;

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

BOOST_AUTO_TEST_SUITE_END()
