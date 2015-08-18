#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LOOKUPLINEARTEST
#include "calc_lookuplineartest.h"
#define INCLUDED_CALC_LOOKUPLINEARTEST
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
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

// Module headers.
#ifndef INCLUDED_CALC_LOOKUPLINEAR
#include "calc_lookuplinear.h"
#define INCLUDED_CALC_LOOKUPLINEAR
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

/*!
  \file
  This file contains the implementation of the LookupLinearTest class.
*/

namespace calc {

class LookupLinearCtor : public LookupLinear {
 public:
  LookupLinearCtor(
        const char *contents,
        VS out,
        std::vector<VS> in):
    LookupLinear(out)
    {
       std::string name("LookupLinearTest.tbl");
       com::write(contents,name);
       setRecords(name,in);
    }
 };
}


//------------------------------------------------------------------------------
// DEFINITION OF STATIC LOOKUPLINEAR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::LookupLinearTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<LookupLinearTest> instance(new LookupLinearTest());

  suite->add(BOOST_CLASS_TEST_CASE(&LookupLinearTest::testOldStyleCtor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LookupLinearTest::testAllIntervals, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LookupLinearTest::testMultipleRecords, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF LOOKUPLINEAR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::LookupLinearTest::LookupLinearTest()
{
}



//! setUp
void calc::LookupLinearTest::setUp()
{
}

//! tearDown
void calc::LookupLinearTest::tearDown()
{
}


static std::vector<double> makeKey(double v1, double v2=-1024)
{
 std::vector<double> k(1,v1);
 if (v2 != -1024)
   k.push_back(v2);
 return k;
}


void calc::LookupLinearTest::testOldStyleCtor()
{
  std::vector<VS> inKeys(1,VS_S);
  double r;

 { // OK
  LookupLinearCtor t("[3 , 5 ] 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(0)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(5.1)));
  BOOST_CHECK( r==-2);
 }

 bool failure=false;
 try { // 2.4 not a nominal
  LookupLinearCtor t("[3 , 5 ] 2.4",VS_N,inKeys);
 } catch (const com::Exception& e) {
   BOOST_CHECK(e.messages().find("2.4")     != std::string::npos &&
             e.messages().find("nominal") != std::string::npos );
   failure=true;
 }
 BOOST_CHECK(failure);


}

void calc::LookupLinearTest::testAllIntervals()
{
  std::vector<VS> inKeys(1,VS_S);
  double r;

 {
  // TEST_ONE new com::EqualTo(l->l)
  LookupLinearCtor t(" 4 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(5.2)));
  BOOST_CHECK( r==-2); r=-2;

 }
 {
  // TEST_INF_INF com::AnthingInterval() infinity
  LookupLinearCtor t("<,>  2.4",VS_S,inKeys);
  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5.2)));
  BOOST_CHECK( r==2.4); r=-2;
 }
 {
  // TEST_GE_INF w com::GreaterThanEqualTo(l->l) [l  ,inf>
  LookupLinearCtor t("[3 ,  ] 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4); r=-2;
 }
 {
   // TEST_GT_INF  com::GreaterThan(l->l);  <l  ,inf>
  LookupLinearCtor t("<3 ,  > 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4); r=-2;
 }
 {
   // TEST_INF_LE  com::LessThanEqualTo(l->h) <inf,h]
  LookupLinearCtor t("<  , 4] 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(2)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(5)));
  BOOST_CHECK( r==-2);
 }
 {
   // TEST_INF_LT : com::LessThan(l->h);        // <inf,h>
  LookupLinearCtor t("<  , 4> 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(2)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(4)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(5)));
  BOOST_CHECK( r==-2);
 }
 {
    // TEST_GE_LE  com::BetweenLimits(
    //                        com::GreaterThanEqualTo(l->l),
    //                        com::LessThanEqualTo(l->h));       [l  ,h]
  LookupLinearCtor t("[3 , 5] 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(6)));
  BOOST_CHECK( r==-2);
 }
 {
    // TEST_GT_LE  com::BetweenLimits(
    //                        com::GreaterThan(l->l),
    //                        com::LessThanEqualTo(l->h));         <l  ,h]
  LookupLinearCtor t("<3 , 5] 2.4",VS_S,inKeys);

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(6)));
  BOOST_CHECK( r==-2);
 }
 {
   // TEST_GE_LT com::BetweenLimits(
   //                         com::GreaterThanEqualTo(l->l),
   //                         com::LessThan(l->h));               [l  ,h>
   LookupLinearCtor t("[3 , 5> 2.4",VS_S,inKeys);

   r=-2;
   BOOST_CHECK(!t.find(r,makeKey(2)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK( t.find(r,makeKey(3)));
   BOOST_CHECK( r==2.4); r=-2;
   BOOST_CHECK( t.find(r,makeKey(4)));
   BOOST_CHECK( r==2.4); r=-2;
   BOOST_CHECK(!t.find(r,makeKey(5)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.find(r,makeKey(6)));
   BOOST_CHECK( r==-2);
 }
 {
   // TEST_GT_LT  com::BetweenLimits(
   //                         com::GreaterThan(l->l),
   //                         com::LessThan(l->h));                <l  ,h>
   LookupLinearCtor t("<3 , 5> 2.4",VS_S,inKeys);

   r=-2;
   BOOST_CHECK(!t.find(r,makeKey(2)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.find(r,makeKey(3)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK( t.find(r,makeKey(4)));
   BOOST_CHECK( r==2.4); r=-2;
   BOOST_CHECK(!t.find(r,makeKey(5)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.find(r,makeKey(6)));
   BOOST_CHECK( r==-2);
 }
}

void calc::LookupLinearTest::testMultipleRecords()
{
 {
  std::vector<VS> inKeys(1,VS_S);
  double r;
  LookupLinearCtor ts(
     " 0.0  0 \n"
     " 3.0  0  \n"
     " 6.0   1 \n"
     " 9.0   1 \n"
     " 12.0  0 \n"
     " 15.0  0 \n" ,VS_S,inKeys);
  r=-2;
  BOOST_CHECK( ts.find(r,makeKey(4.5)));
  BOOST_CHECK( r==0.5); r=-2;
  BOOST_CHECK( ts.find(r,makeKey(7)));
  BOOST_CHECK( r==1);   r=-2;
  BOOST_CHECK(!ts.find(r,makeKey(-999999999)));
  BOOST_CHECK(!ts.find(r,makeKey(999999999)));

  BOOST_CHECK( ts.find(r,makeKey(14)));
  BOOST_CHECK( r==0);   r=-2;

  BOOST_CHECK( ts.find(r,makeKey(1)));
  BOOST_CHECK( r==0);   r=-2;
 }
 {
  std::vector<VS> inKeys(1,VS_S);
  double r;
  LookupLinearCtor t("[3 , 5 ]  2.4\n"
                                " 8         4.8",VS_S,inKeys);
  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(8)));
  BOOST_CHECK( r==4.8); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(-999999999)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(6.5)));
  BOOST_CHECK(com::equal_epsilon(r,3.6));
 }
 {
  std::vector<VS> inKeys(1,VS_S);
  double r;
  LookupLinearCtor t("4  2\n"
                                " 8  4",VS_S,inKeys);
  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( r==-2 );
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2  ); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.5);
  BOOST_CHECK( t.find(r,makeKey(8)));
  BOOST_CHECK( r==4  ); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(10)));
  BOOST_CHECK( r==-2 );
 }
}
