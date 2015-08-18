#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLETEST
#include "calc_lookuptabletest.h"
#define INCLUDED_CALC_LOOKUPTABLETEST
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

// Module headers.
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

/*!
  \file
  This file contains the implementation of the LookupTableTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LOOKUPTABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::LookupTableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<LookupTableTest> instance(new LookupTableTest());

  suite->add(BOOST_CLASS_TEST_CASE(&LookupTableTest::testOldStyleCtor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LookupTableTest::testAllIntervals, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LookupTableTest::testMultipleKeys, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LookupTableTest::testMultipleRecords, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF LOOKUPTABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::LookupTableTest::LookupTableTest()
{
}



//! setUp
void calc::LookupTableTest::setUp()
{
}

//! tearDown
void calc::LookupTableTest::tearDown()
{
}

void createTestTable(
    calc::LookupTable& t,
    const char *contents,
    const std::vector<VS>& inKeys)
{
  std::string name("LookupTableTest.tbl");
  com::write(contents,name);
  t.setRecords(name,inKeys);
}

static std::vector<double> makeKey(double v1, double v2=-1024)
{
 std::vector<double> k(1,v1);
 if (v2 != -1024)
   k.push_back(v2);
 return k;
}


void calc::LookupTableTest::testOldStyleCtor()
{
  std::vector<VS> inKeys(1,VS_S);
  double r;

 { // OK
  LookupTable t(VS_S);
  createTestTable(t,"[3 , 5 ] 2.4",inKeys);

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
  LookupTable t(VS_N);
  createTestTable(t,"[3 , 5 ] 2.4",inKeys);
 } catch (const com::Exception& e) {
   BOOST_CHECK(e.messages().find("2.4")     != std::string::npos &&
             e.messages().find("nominal") != std::string::npos );
   failure=true;
 }
 BOOST_CHECK(failure);


}

void calc::LookupTableTest::testAllIntervals()
{
  std::vector<VS> inKeys(1,VS_S);
  double r;

 {
  // TEST_ONE new com::EqualTo(l->l)
  LookupTable t(VS_S);
  createTestTable(t," 4 2.4",inKeys);

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(5.2)));
  BOOST_CHECK( r==-2); r=-2;

 }
 {
  // TEST_INF_INF com::AnthingInterval() infinity
  LookupTable t(VS_S);
  createTestTable(t,"<,>  2.4",inKeys);
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
  LookupTable t(VS_S);
  createTestTable(t,"[3 ,  ] 2.4",inKeys);

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
  LookupTable t(VS_S);
  createTestTable(t,"<3 ,  > 2.4",inKeys);

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
  LookupTable t(VS_S);
  createTestTable(t,"<  , 4] 2.4",inKeys);

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
  LookupTable t(VS_S);
  createTestTable(t,"<  , 4> 2.4",inKeys);

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
  LookupTable t(VS_S);
  createTestTable(t,"[3 , 5] 2.4",inKeys);

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
  LookupTable t(VS_S);
  createTestTable(t,"<3 , 5] 2.4",inKeys);

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
   LookupTable t(VS_S);
   createTestTable(t,"[3 , 5> 2.4",inKeys);

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
   LookupTable t(VS_S);
   createTestTable(t,"<3 , 5> 2.4",inKeys);

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

void calc::LookupTableTest::testMultipleKeys()
{
  std::vector<VS> inKeys(2,VS_S);
  double r;
  LookupTable t(VS_S);
  createTestTable(t,"[3 , 5 ] [ 7, 9] 2.4",inKeys);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3,8)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4,8)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5,8)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(8,4)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(2,10)));
  BOOST_CHECK( r==-2);
}

void calc::LookupTableTest::testMultipleRecords()
{
  std::vector<VS> inKeys(2,VS_S);
  double r;
  LookupTable t(VS_S);
  createTestTable(t,"[3 , 5 ] [ 7, 9] 2.4\n"
                    " 8       [ 7, 9] 4.8\n"
                    " <,>     [ 7, 9] 8  \n" ,inKeys);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3,8)));
  BOOST_CHECK( r==2.4); r=-2;
  BOOST_CHECK( t.find(r,makeKey(8,8)));
  BOOST_CHECK( r==4.8); r=-2;
  BOOST_CHECK( t.find(r,makeKey(-999999999,8)));
  BOOST_CHECK( r==8); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2,10)));
  BOOST_CHECK( r==-2);
}
