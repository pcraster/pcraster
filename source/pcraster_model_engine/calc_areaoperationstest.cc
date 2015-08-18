#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_AREAOPERATIONSTEST
#include "calc_areaoperationstest.h"
#define INCLUDED_CALC_AREAOPERATIONSTEST
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
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

// Module headers.
#ifndef INCLUDED_CALC_AREAOPERATIONS
#include "calc_areaoperations.h"
#define INCLUDED_CALC_AREAOPERATIONS
#endif
#ifndef INCLUDED_CALC_ORDEROPERATIONS
#include "calc_orderoperations.h"
#define INCLUDED_CALC_ORDEROPERATIONS
#endif



/*!
  \file
  This file contains the implementation of the AreaOperationsTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC AREAOPERATIONS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::AreaOperationsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AreaOperationsTest> instance(new AreaOperationsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AreaOperationsTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&AreaOperationsTest::testAreaOrder, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF AREAOPERATIONS MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::AreaOperationsTest::AreaOperationsTest()
{
}



//! setUp
void calc::AreaOperationsTest::setUp()
{
}



//! tearDown
void calc::AreaOperationsTest::tearDown()
{
}

void calc::AreaOperationsTest::test()
{
  {
   AreaTotalOperation ao;
   UINT1 id[1]  = { 1 };
   REAL4 val[1] = { 8 };
   ao.apply(val,id,1);
   BOOST_CHECK(ao.d_map.size()==1);
   BOOST_CHECK(val[0]==8);
  }
  {
   AreaTotalOperation ao;

   UINT1 id[4]  = { 1, 1, MV_UINT1, 2  };
   REAL4 val[4] = { 8, 8, 100,      10 };

   REAL4 res[4] = {16,16, 100,      10 };
   pcr::setMV(res[2]);

   ao.apply(val,id,4);
   BOOST_CHECK(ao.d_map.size()==2);
   BOOST_CHECK(ao.d_map[1].nr()==2);
   BOOST_CHECK(ao.d_map[2].nr()==1);
   BOOST_CHECK(ao.d_map[1].sum()==16);
   BOOST_CHECK(ao.d_map[2].sum()==10);

   BOOST_CHECK(val[0]==16);
   BOOST_CHECK(val[1]==16);
   BOOST_CHECK(pcr::isMV(val[2]));
   BOOST_CHECK(val[3]==10);

  }
/*
  {
   AreaTotalOperation ao;
   UINT1 id[4]  = { 1, 1, MV_UINT1, 2 };
   REAL4 val[4] = { 8, 4, 4,        1 };
   ao.apply(id,4,val,4);
   BOOST_CHECK(ao.d_map.size()==2);
   BOOST_CHECK(ao.d_map[1].nr()==2);
   BOOST_CHECK(ao.d_map[2].nr()==1);
   double res[2]={-1,-1};
   ao.setResults(res,2);
   BOOST_CHECK(res[0]==6);
   BOOST_CHECK(res[1]==1);
  }
*/
}

void calc::AreaOperationsTest::testAreaOrder()
{
   REAL4 exprV[9]      = { 9,2,3,4,5,6,7,8       ,-999 };
   pcr::setMV(exprV[8]);

   UINT1 areaClassV[9] = { 1,1,0,0,0,0,1,MV_UINT1,0 };
   REAL4 expectedV[9]  = { 3,1,1,2,3,4,2,-999    ,0 };
   REAL4 result[9]     = { 0,0,0,0,0,0,0,0       ,0 };

   VSpatial<double,REAL4> expr(exprV);
   VSpatial<INT4,UINT1>   areaClass(areaClassV);

   areaOrderOperation(result, expr,areaClass,9);

   for(size_t i=0; i< 7 ; ++i)
    BOOST_CHECK(expectedV[i]==result[i]);
   BOOST_CHECK(pcr::isMV(result[7]));
   BOOST_CHECK(pcr::isMV(result[8]));
}
