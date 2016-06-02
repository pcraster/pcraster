#define BOOST_TEST_MODULE pcraster model_engine argorder
#include <boost/test/unit_test.hpp>
#include "calc_argorder.h"
#include "pcrtypes.h"


BOOST_AUTO_TEST_CASE(testArgOrderAL)
{
  using namespace calc;

  { // no args, does test the case
    //  where all areaLimit's are smaller then 1
    INT4  result[1]  = {-1 };
    std::vector<ArgOrderIdInfo> args;
    ArgOrderAndAddArea::argOrderAreaLimited(args,result,1);
    BOOST_CHECK(result[0]==0);
  }
  { // ALL MV
    REAL4 chance1[1] = {-1};
    pcr::setMV(chance1[0]);
    INT4  result[1]  = {-1 };
    std::vector<ArgOrderIdInfo> args;
    args.push_back(ArgOrderIdInfo(chance1,2,2.4));
    ArgOrderAndAddArea::argOrderAreaLimited(args,result,1);
    BOOST_CHECK(result[0] == MV_INT4);
  }
  {
    REAL4 chance1[5] = {0.1F,0.2F,0.3F,0.4F,0.5F};
    INT4  result[5]  = {-1 ,-1 ,-1 ,-1 ,-1 };
    std::vector<ArgOrderIdInfo> args;
    args.push_back(ArgOrderIdInfo(chance1,2,2.4));
    ArgOrderAndAddArea::argOrderAreaLimited(args,result,5);
    BOOST_CHECK(result[0] == 0);
    BOOST_CHECK(result[1] == 0);
    BOOST_CHECK(result[2] == 0);
    BOOST_CHECK(result[3] == 2);
    BOOST_CHECK(result[4] == 2);
  }
  { // some MV
    REAL4 chance1[5] = {0.1F,0.2F,0.3F,0.4F,0.5F};
    pcr::setMV(chance1[3]);
    INT4  result[5]  = {-1 ,-1 ,-1 ,-1 ,-1 };
    std::vector<ArgOrderIdInfo> args;
    args.push_back(ArgOrderIdInfo(chance1,2,2.4));
    ArgOrderAndAddArea::argOrderAreaLimited(args,result,5);
    BOOST_CHECK(result[0] == 0);
    BOOST_CHECK(result[1] == 0);
    BOOST_CHECK(result[2] == 2);
    BOOST_CHECK(result[3] == MV_INT4);
    BOOST_CHECK(result[4] == 2);
  }
  { // some MV, 2 lists
    REAL4 chance1[9] = {0.1F,0.2F,0.3F ,0.4F,0.5F ,0.6F,0.0F,0.0F,0.0F};
    pcr::setMV(chance1[3]);
    REAL4 chance2[9] = {0.1F,0.2F,0.33F,0.4F,0.55F,0.6F,0,0,0};
    pcr::setMV(chance2[1]);
    INT4  result[9]  = {-1 ,-1 ,-1 ,-1 ,-1,-1 };
    std::vector<ArgOrderIdInfo> args;
    args.push_back(ArgOrderIdInfo(chance1,11,2));
    args.push_back(ArgOrderIdInfo(chance2,12,2));
    ArgOrderAndAddArea::argOrderAreaLimited(args,result,9);
    BOOST_CHECK(result[0] == 11);
    BOOST_CHECK(result[1] == MV_INT4);
    BOOST_CHECK(result[2] == 12);
    BOOST_CHECK(result[3] == MV_INT4);
    BOOST_CHECK(result[4] == 12);
    BOOST_CHECK(result[5] == 11); // stride take first
    BOOST_CHECK(result[6] == 0);
    BOOST_CHECK(result[7] == 0);
    BOOST_CHECK(result[8] == 0);
  }
}

BOOST_AUTO_TEST_CASE(testArgOrder)
{
  using namespace calc;

  { // no args, can not happen with argorder
    // but safety first
    INT4  result[1]  = {-1 };
    std::vector<ArgOrderIdInfo> args;
    ArgOrderAndAddArea::argOrder(args,result,1);
    BOOST_CHECK(result[0]==MV_INT4);
  }
  { // ALL MV
    REAL4 chance1[1] = {-1};
    pcr::setMV(chance1[0]);
    INT4  result[1]  = {-1 };
    std::vector<ArgOrderIdInfo> args;
    args.push_back(ArgOrderIdInfo(chance1,2));
    ArgOrderAndAddArea::argOrder(args,result,1);
    BOOST_CHECK(result[0] == MV_INT4);
  }
  { // some MV, 2 lists
    REAL4 chance1[9] = {0.1F,0.2F,0.3F ,0.4F,0.5F ,0.6F,0.0F,0.0F,0.0F};
    pcr::setMV(chance1[3]);
    REAL4 chance2[9] = {0.1F,0.2F,0.33F,0.4F,0.55F,0.6F,0.0F,0.0F,0.0F};
    pcr::setMV(chance2[1]);
    INT4  result[9]  = {-1 ,-1 ,-1 ,-1 ,-1,-1,-1,-1,-1 };
    std::vector<ArgOrderIdInfo> args;
    args.push_back(ArgOrderIdInfo(chance1,11));
    args.push_back(ArgOrderIdInfo(chance2,12));

    ArgOrderAndAddArea::argOrder(args,result,9);

    BOOST_CHECK(result[0] == 11);
    BOOST_CHECK(result[1] == MV_INT4);
    BOOST_CHECK(result[2] == 12);
    BOOST_CHECK(result[3] == MV_INT4);
    BOOST_CHECK(result[4] == 12);
    BOOST_CHECK(result[5] == 11); // stride take first
    BOOST_CHECK(result[6] == 11);
    BOOST_CHECK(result[7] == 11);
    BOOST_CHECK(result[8] == 11);
  }
}

BOOST_AUTO_TEST_CASE(testAddArea)
{
  using namespace calc;

  // SEE RIVM Notebook page 44
  // 1 -> Recreation
  // 2 -> Residential
  // 3 -> Agriculture
  //                   1   2    3    4    5    6    7    8
  INT4 currentId[8]= { 3  ,3   ,3   ,1   ,1   ,3   ,3   ,3 };
  INT4    result[8]= {-1  ,-1  ,-1  ,-1  ,-1  ,-1  ,-1  ,-1};
  std::vector<ArgOrderIdInfo> args;

  REAL4  chance1[8]= {0.99F,0.65F,0.99F,0.92F,0.92F,0.99F,0.11F,0.09F};
  args.push_back(ArgOrderIdInfo(chance1,1,1));

  REAL4  chance2[8]= {0.87F,0.99F,0.98F,0.97F,0.38F,0.98F,0.21F,0.12F};
  args.push_back(ArgOrderIdInfo(chance2,2,4));

  ArgOrderAndAddArea::argOrderAddAreaLimited(args,currentId,result,8);
  BOOST_CHECK(result[0]==1);
  BOOST_CHECK(result[1]==2);
  BOOST_CHECK(result[2]==1);
  BOOST_CHECK(result[3]==2);
  BOOST_CHECK(result[4]==2);
  BOOST_CHECK(result[5]==1);
  BOOST_CHECK(result[6]==2);
  BOOST_CHECK(result[7]==0);
}
/*
1:1
1:2
1:2
1:4
1:5
1:5
1:7
1:8
0]:1
1]:2
2]:1
3]:2
4]:1
5]:1
6]:2
7]:2
*/

