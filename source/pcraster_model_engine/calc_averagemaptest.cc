#define BOOST_TEST_MODULE pcraster model_engine averagemap
#include <boost/test/unit_test.hpp>

#define private public
#include "calc_averagemap.h"


// NOTE use string failureExpected in files expected to fail, see style guide


BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

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
