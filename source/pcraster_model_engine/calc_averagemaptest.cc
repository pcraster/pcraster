#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_AVERAGEMAPTEST
#include "calc_averagemaptest.h"
#define INCLUDED_CALC_AVERAGEMAPTEST
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
#ifndef INCLUDED_CALC_AVERAGEMAP
#include "calc_averagemap.h"
#define INCLUDED_CALC_AVERAGEMAP
#endif


/*!
  \file
  This file contains the implementation of the AverageMapTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC AVERAGEMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::AverageMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AverageMapTest> instance(new AverageMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AverageMapTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF AVERAGEMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::AverageMapTest::AverageMapTest()
{
}



//! setUp
void calc::AverageMapTest::setUp()
{
}



//! tearDown
void calc::AverageMapTest::tearDown()
{
}

void calc::AverageMapTest::test()
{
  {
   AverageMap am;
   UINT1 id[1]  = { 1 };
   REAL4 val[1] = { 8 };
   am.apply(id,1,val,1);
   BOOST_CHECK(am.d_map.size()==1);
   double res[1]={-1};
   am.setResults(res,1);
   BOOST_CHECK(res[0]==8);
  }
  {
   AverageMap am;
   UINT1 id[4]  = { 1, 1, MV_UINT1, 2 };
   REAL4 val[1] = { 8 };
   am.apply(id,4,val,1);
   BOOST_CHECK(am.d_map.size()==2);
   BOOST_CHECK(am.d_map[1].nr()==2);
   BOOST_CHECK(am.d_map[2].nr()==1);
   double res[2]={-1,-1};
   am.setResults(res,2);
   BOOST_CHECK(res[0]==8);
   BOOST_CHECK(res[1]==8);
  }
  {
   AverageMap am;
   UINT1 id[4]  = { 1, 1, MV_UINT1, 2 };
   REAL4 val[4] = { 8, 4, 4,        1 };
   am.apply(id,4,val,4);
   BOOST_CHECK(am.d_map.size()==2);
   BOOST_CHECK(am.d_map[1].nr()==2);
   BOOST_CHECK(am.d_map[2].nr()==1);
   double res[2]={-1,-1};
   am.setResults(res,2);
   BOOST_CHECK(res[0]==6);
   BOOST_CHECK(res[1]==1);
  }
}
