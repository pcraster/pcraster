#define BOOST_TEST_MODULE pcraster model_engine areaoperations
#include <boost/test/unit_test.hpp>
#include "com_csfcell.h"
#include "calc_orderoperations.h"

#define protected public
#include "calc_areaoperations.h"



// NOTE use string failureExpected in files expected to fail, see style guide




BOOST_AUTO_TEST_CASE(testA)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testAreaOrder)
{
  using namespace calc;

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
