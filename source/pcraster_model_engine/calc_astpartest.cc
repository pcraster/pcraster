#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTPARTEST
#include "calc_astpartest.h"
#define INCLUDED_CALC_ASTPARTEST
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

// Module headers.
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_PARSET
#include "calc_parset.h"
#define INCLUDED_CALC_PARSET
#endif
/*!
  \file
  This file contains the implementation of the ASTParTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTPAR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ASTParTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ASTParTest> instance(new ASTParTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ASTParTest::testCompare, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ASTParTest::testSet, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ASTPAR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ASTParTest::ASTParTest()
{
}



//! setUp
void calc::ASTParTest::setUp()
{
}

//! tearDown
void calc::ASTParTest::tearDown()
{
}

void calc::ASTParTest::testCompare()
{
  ASTPar a("aName"),b("bName");
  BOOST_CHECK(a!=b);
  BOOST_CHECK(a==a);
  BOOST_CHECK(b==b);
  BOOST_CHECK(a<b);

  ASTPar ai1(a);
  ai1.pushBackIndex(TmpId("index1"));

  BOOST_CHECK(ai1==ai1);
  BOOST_CHECK(a!=ai1);
  BOOST_CHECK(a<ai1);
  BOOST_CHECK(ai1<b);

  ASTPar ai2(a);
  ai2.pushBackIndex(TmpId("index2"));
  BOOST_CHECK(ai2==ai2);
  BOOST_CHECK(ai1!=ai2);
  BOOST_CHECK(ai1<ai2);
}

void calc::ASTParTest::testSet()
{
  ASTPar a1("a");
  ASTPar a2("a");
  ParSet s1;
  s1.insert(&a1);
  BOOST_CHECK(s1.find(&a1)==&a1);
  s1.insert(&a2);
  // find first inserted
  BOOST_CHECK(s1.find(&a2)==&a1);
  BOOST_CHECK(s1.size()==1);
  ASTPar v1("v");
  s1.insert(&v1);
  BOOST_CHECK(s1.size()==2);

  BOOST_CHECK(  s1 == s1);
  BOOST_CHECK(!(s1 != s1));

  ParSet s2;
  ASTPar a3("a");

  BOOST_CHECK(s2.count(&a3)==0);
  s2.insert(&a3);
  BOOST_CHECK(s2.count(&a3)==1);

  BOOST_CHECK(  s1 != s2);
  BOOST_CHECK(!(s1 == s2));

  ASTPar v2("v");
  s2.insert(&v2);

  BOOST_CHECK(s2.size()==2);
  BOOST_CHECK(!(s1 != s2));
  BOOST_CHECK(  s1 == s2);

  ASTPar b1("b");
  BOOST_CHECK(s2.find(&b1)==0);


  s2.insert(&b1);

  BOOST_CHECK(s2.find(&b1)==&b1);

  std::vector<calc::ASTPar *> vector=s2.toSortedVector();
  BOOST_CHECK(vector.size()==3);
  BOOST_CHECK(vector[0]->name()=="a");
  BOOST_CHECK(vector[1]->name()=="b");
  BOOST_CHECK(vector[2]->name()=="v");
}
