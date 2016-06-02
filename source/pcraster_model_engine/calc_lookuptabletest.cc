#define BOOST_TEST_MODULE pcraster model_engine lookuptable
#include <boost/test/unit_test.hpp>
#include "com_pathname.h"
#include "com_exception.h"
#include "com_file.h"
#include "com_math.h"
#include "calc_vs.h"
#include "calc_globallibdefs.h"

#define private public
#include "calc_lookuptable.h"





//------------------------------------------------------------------------------
// DEFINITION OF STATIC LOOKUPTABLE MEMBERS
//------------------------------------------------------------------------------
namespace calc {
class TestLookupCtor : public LookupTable {
 public:
  TestLookupCtor(
        const char *contents,
        std::vector<VS> vs= std::vector<VS>())
    {
       if (vs.empty())
         vs=std::vector<VS>(2,VS_S);
       std::string name("LookupTableTest.tbl");
       com::write(contents,name);
       setRecords(name,vs);
    }
 };

static LookupTable::Key makeKey(float v1, float v2=-1024)
{
  LookupTable::Key k(1,v1);
 if (v2 != -1024)
   k.push_back(v2);
 return k;
}
}



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

BOOST_FIXTURE_TEST_SUITE(lookuptable, Fixture)



BOOST_AUTO_TEST_CASE(testOldStyleCtor)
{
  using namespace calc;

  double r;

 { // OK
  std::vector<VS> colVs(2,VS_S);
  TestLookupCtor t("[3 , 5 ] 2.4",colVs);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(4.0f)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(3.0f)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5.0f)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(0.0f)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(5.1f)));
  BOOST_CHECK( r==-2);

  r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(3.0f)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(4.0f)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5.0f)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(0)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.interpolate(r,makeKey(5.1f)));
  BOOST_CHECK( r==-2);
 }
 { // OK
  std::vector<VS> colVs(2,VS_S);
  TestLookupCtor t("<0  , 0.2 ] 1\n"\
                   "<0.2, 0.4 ] 2" ,colVs);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(0.2f)));
  BOOST_CHECK( r==1); r=-2;
 }

 bool failure=false;
 try { // 2.4 not a nominal
  std::vector<VS> colVs(2,VS_S);
  colVs[1]=VS_N;
  TestLookupCtor t("[3 , 5 ] 2.4",colVs);
 } catch (const com::Exception& e) {
   BOOST_CHECK(e.messages().find("2.4")     != std::string::npos &&
             e.messages().find("nominal") != std::string::npos );
   failure=true;
 }
 BOOST_CHECK(failure);


}

BOOST_AUTO_TEST_CASE(testAllIntervals)
{
  using namespace calc;

  double r;

 {
  // TEST_ONE new com::EqualTo(l->l)
  TestLookupCtor t(" 4 2.4");

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(5.2f)));
  BOOST_CHECK( r==-2); r=-2;

 }
 {
  // TEST_INF_INF com::AnythingValidator() infinity
  TestLookupCtor t("<,>  2.4");
  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5.2f)));
  BOOST_CHECK( r==2.4f); r=-2;
 }
 {
  // TEST_GE_INF w com::GreaterThanEqualTo(l->l) [l  ,inf>
  TestLookupCtor t("[3 ,  ] 2.4");

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
 }
 {
   // TEST_GT_INF  com::GreaterThan(l->l);  <l  ,inf>
  TestLookupCtor t("<3 ,  > 2.4");

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
 }
 {
   // TEST_INF_LE  com::LessThanEqualTo(l->h) <inf,h]
  TestLookupCtor t("<  , 4] 2.4");

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(2)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(5)));
  BOOST_CHECK( r==-2);
 }
 {
   // TEST_INF_LT : com::LessThan(l->h);        // <inf,h>
  TestLookupCtor t("<  , 4> 2.4");

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(2)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(4)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(5)));
  BOOST_CHECK( r==-2);
 }
 {
    // TEST_GE_LE  com::BetweenLimits(
    //                        com::GreaterThanEqualTo(l->l),
    //                        com::LessThanEqualTo(l->h));       [l  ,h]
  TestLookupCtor  t("[3 , 5] 2.4");

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(6)));
  BOOST_CHECK( r==-2);
 }
 {
    // TEST_GT_LE  com::BetweenLimits(
    //                        com::GreaterThan(l->l),
    //                        com::LessThanEqualTo(l->h));         <l  ,h]
  TestLookupCtor t("<3 , 5] 2.4");

  r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(3)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.find(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(6)));
  BOOST_CHECK( r==-2);
 }
 {
   // TEST_GE_LT com::BetweenLimits(
   //                         com::GreaterThanEqualTo(l->l),
   //                         com::LessThan(l->h));               [l  ,h>
   TestLookupCtor t("[3 , 5> 2.4");

   r=-2;
   BOOST_CHECK(!t.find(r,makeKey(2)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK( t.find(r,makeKey(3)));
   BOOST_CHECK( r==2.4f); r=-2;
   BOOST_CHECK( t.find(r,makeKey(4)));
   BOOST_CHECK( r==2.4f); r=-2;
   BOOST_CHECK(!t.find(r,makeKey(5)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.find(r,makeKey(6)));
   BOOST_CHECK( r==-2);
 }
 {
   // TEST_GT_LT  com::BetweenLimits(
   //                         com::GreaterThan(l->l),
   //                         com::LessThan(l->h));                <l  ,h>
   TestLookupCtor t("<3 , 5> 2.4");

   r=-2;
   BOOST_CHECK(!t.find(r,makeKey(2)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.find(r,makeKey(3)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK( t.find(r,makeKey(4)));
   BOOST_CHECK( r==2.4f); r=-2;
   BOOST_CHECK(!t.find(r,makeKey(5)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.find(r,makeKey(6)));
   BOOST_CHECK( r==-2);
 }
}

//! this set of tests is exactly identical to testAllIntervals()
BOOST_AUTO_TEST_CASE(testAllIntervalsInterpolate)
{
  using namespace calc;

  double r;

 {
  // TEST_ONE new com::EqualTo(l->l)
  TestLookupCtor t(" 4 2.4");

  r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(3)));
  BOOST_CHECK( t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(5.2f)));
  BOOST_CHECK( r==-2); r=-2;

 }
 {
  // TEST_INF_INF com::AnythingValidator() infinity
  TestLookupCtor t("<,>  2.4");
  r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5.2f)));
  BOOST_CHECK( r==2.4f); r=-2;
 }
 {
  // TEST_GE_INF w com::GreaterThanEqualTo(l->l) [l  ,inf>
  TestLookupCtor t("[3 ,  ] 2.4");

  r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
 }
 {
   // TEST_GT_INF  com::GreaterThan(l->l);  <l  ,inf>
  TestLookupCtor t("<3 ,  > 2.4");

  r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
 }
 {
   // TEST_INF_LE  com::LessThanEqualTo(l->h) <inf,h]
  TestLookupCtor t("<  , 4] 2.4");

  r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(2)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==-2);
 }
 {
   // TEST_INF_LT : com::LessThan(l->h);        // <inf,h>
  TestLookupCtor t("<  , 4> 2.4");

  r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(2)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==-2);
 }
 {
    // TEST_GE_LE  com::BetweenLimits(
    //                        com::GreaterThanEqualTo(l->l),
    //                        com::LessThanEqualTo(l->h));       [l  ,h]
  TestLookupCtor t("[3 , 5] 2.4");

  r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(6)));
  BOOST_CHECK( r==-2);
 }
 {
    // TEST_GT_LE  com::BetweenLimits(
    //                        com::GreaterThan(l->l),
    //                        com::LessThanEqualTo(l->h));         <l  ,h]
  TestLookupCtor t("<3 , 5] 2.4");

  r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(2)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(6)));
  BOOST_CHECK( r==-2);
 }
 {
   // TEST_GE_LT com::BetweenLimits(
   //                         com::GreaterThanEqualTo(l->l),
   //                         com::LessThan(l->h));               [l  ,h>
   TestLookupCtor t("[3 , 5> 2.4");

   r=-2;
   BOOST_CHECK(!t.interpolate(r,makeKey(2)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK( t.interpolate(r,makeKey(3)));
   BOOST_CHECK( r==2.4f); r=-2;
   BOOST_CHECK( t.interpolate(r,makeKey(4)));
   BOOST_CHECK( r==2.4f); r=-2;
   BOOST_CHECK(!t.interpolate(r,makeKey(5)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.interpolate(r,makeKey(6)));
   BOOST_CHECK( r==-2);
 }
 {
   // TEST_GT_LT  com::BetweenLimits(
   //                         com::GreaterThan(l->l),
   //                         com::LessThan(l->h));                <l  ,h>
   TestLookupCtor t("<3 , 5> 2.4");

   r=-2;
   BOOST_CHECK(!t.interpolate(r,makeKey(2)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.interpolate(r,makeKey(3)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK( t.interpolate(r,makeKey(4)));
   BOOST_CHECK( r==2.4f); r=-2;
   BOOST_CHECK(!t.interpolate(r,makeKey(5)));
   BOOST_CHECK( r==-2);
   BOOST_CHECK(!t.interpolate(r,makeKey(6)));
   BOOST_CHECK( r==-2);
 }
}

BOOST_AUTO_TEST_CASE(testMultipleKeys)
{
  using namespace calc;

  std::vector<VS> colVs(3,VS_S);
  double r;
  TestLookupCtor t("[3 , 5 ] [ 7, 9] 2.4",colVs);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3,8)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(4,8)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(5,8)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(8,4)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK(!t.find(r,makeKey(2,10)));
  BOOST_CHECK( r==-2);
}

BOOST_AUTO_TEST_CASE(testMultipleRecords)
{
  using namespace calc;

  std::vector<VS> colVs(3,VS_S);
  double r;
  TestLookupCtor t("[3 , 5 ] [ 7, 9] 2.4\n"
               " 8       [ 7, 9] 4.8\n"
               " <,>     [ 7, 9] 8  \n" ,colVs);

  r=-2;
  BOOST_CHECK( t.find(r,makeKey(3,8)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(8,8)));
  BOOST_CHECK( r==4.8f); r=-2;
  BOOST_CHECK( t.find(r,makeKey(-999999999.0f,8)));
  BOOST_CHECK( r==8); r=-2;
  BOOST_CHECK(!t.find(r,makeKey(2,10)));
  BOOST_CHECK( r==-2);
}

BOOST_AUTO_TEST_CASE(testInterpolate)
{
  using namespace calc;

 {
  double r;
  TestLookupCtor ts(
     " 0.0  0 \n"
     " 3.0  0  \n"
     " 6.0   1 \n"
     " 9.0   1 \n"
     " 12.0  0 \n"
     " 15.0  0 \n" );
  r=-2;
  BOOST_CHECK( ts.interpolate(r,makeKey(4.5)));
  BOOST_CHECK( r==0.5); r=-2;
  BOOST_CHECK( ts.interpolate(r,makeKey(7)));
  BOOST_CHECK( r==1);   r=-2;
  BOOST_CHECK(!ts.interpolate(r,makeKey(-999999999.0f)));
  BOOST_CHECK(!ts.interpolate(r,makeKey(999999999.0f)));

  BOOST_CHECK( ts.interpolate(r,makeKey(14)));
  BOOST_CHECK( r==0);   r=-2;

  BOOST_CHECK( ts.interpolate(r,makeKey(1)));
  BOOST_CHECK( r==0);   r=-2;
 }
 {
  double r;
  TestLookupCtor t("[3 , 5 ]  2.4\n"
               " 8         4.8");
  r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==2.4f); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(8)));
  BOOST_CHECK( r==4.8f); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(-999999999.0f)));
  BOOST_CHECK( r==-2);
  BOOST_CHECK( t.interpolate(r,makeKey(6.5)));
  BOOST_CHECK(com::equal_epsilon(r,3.6));
 }
 {
  double r;
  TestLookupCtor t("4  2\n"
                                " 8  4");
  r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(3)));
  BOOST_CHECK( r==-2 );
  BOOST_CHECK( t.interpolate(r,makeKey(4)));
  BOOST_CHECK( r==2  ); r=-2;
  BOOST_CHECK( t.interpolate(r,makeKey(5)));
  BOOST_CHECK( r==2.5);
  BOOST_CHECK( t.interpolate(r,makeKey(8)));
  BOOST_CHECK( r==4  ); r=-2;
  BOOST_CHECK(!t.interpolate(r,makeKey(10)));
  BOOST_CHECK( r==-2 );
 }
}

BOOST_AUTO_TEST_CASE(testInterpolateMWF)
{
 //using namespace calc;

  // test only prefixKey selection here
  // prefixSize is fixed on 1
  // other in testInterpolate()
 struct MWFTest : public calc::TestLookupCtor {

   calc::RelationRecord::Float d_prefixKeyValue;
   size_t                      d_keyCol;
   size_t                      d_resultCol;
   MWFTest(
      const char *table,
      size_t nrCols):
    TestLookupCtor(table,
        std::vector<VS>(nrCols,VS_S)),
     d_prefixKeyValue(4),
     d_keyCol(1),
     d_resultCol(2)
   {
     setPrefixStableSort(1);
   }
   bool interpolate(double& r,
                    double keyValue) const
   {
     LookupTable::Key prefixKey(1,d_prefixKeyValue);
    return TestLookupCtor::interpolate(r,prefixKey,keyValue,d_keyCol,d_resultCol);
   }
 };

 using namespace calc;

 {
  MWFTest ts(
     "4 0.0  0 \n"
     "4 3.0  0  \n"
     "4 6.0  1 \n"
     "4 9.0  1 \n"
     "4 12.0 0 \n"
     "4 15.0 0 \n",3);
  double r;
  r=-2;
  BOOST_CHECK( ts.interpolate(r,4.5));
  BOOST_CHECK( r==0.5); r=-2;
  BOOST_CHECK( ts.interpolate(r,(7)));
  BOOST_CHECK( r==1);   r=-2;
  BOOST_CHECK(!ts.interpolate(r,(-999999999)));
  BOOST_CHECK(!ts.interpolate(r,(999999999)));

  BOOST_CHECK( ts.interpolate(r,(14)));
  BOOST_CHECK( r==0);   r=-2;

  BOOST_CHECK( ts.interpolate(r,(1)));
  BOOST_CHECK( r==0);   r=-2;

  // none found prefix key empty
  ts.d_prefixKeyValue = 10;
  BOOST_CHECK(!ts.interpolate(r,4.5));
  BOOST_CHECK( r==-2); r=-2;
 }
 {
  MWFTest ts(
     "4 0 0.0  -100 \n"
     "4 0 3.0  -100  \n"
     "4 1 6.0  -100 \n"
     "4 1 9.0  -100 \n"
     "4 0 12.0 -100 \n"
     "4 0 15.0 -100 \n",4);
  ts.d_keyCol=2;
  ts.d_resultCol=1;
  double r;
  r=-2;
  BOOST_CHECK( ts.interpolate(r,4.5));
  BOOST_CHECK( r==0.5); r=-2;
  BOOST_CHECK( ts.interpolate(r,(7)));
  BOOST_CHECK( r==1);   r=-2;
  BOOST_CHECK(!ts.interpolate(r,(-999999999)));
  BOOST_CHECK(!ts.interpolate(r,(999999999)));

  BOOST_CHECK( ts.interpolate(r,(14)));
  BOOST_CHECK( r==0);   r=-2;

  BOOST_CHECK( ts.interpolate(r,(1)));
  BOOST_CHECK( r==0);   r=-2;

  // none found prefix key empty
  ts.d_prefixKeyValue = 10;
  BOOST_CHECK(!ts.interpolate(r,4.5));
  BOOST_CHECK( r==-2); r=-2;
 }
 {
  MWFTest ts(
     "9 4 9.0  -100 \n"       // 7
     "9 4 9.0  -99  \n"       // 8
     "4 0 0.0  -100 \n"       // 0
     "4 0 3.0  -101  \n"      // 1
     "4 1 6.0  -102 \n"       // 2
     "4 1 9.0  -103 \n"       // 3
     "4 0 12.0 -104 \n"       // 4
     "4 0 15.0 -105 \n"       // 5
     "8 9 95.0 -100 \n",4);   // 6
  std::vector<VS> l4s(4,VS_S);
  TestLookupCtor stableSorted(
     "4 0 0.0  -100 \n"       // 0
     "4 0 3.0  -101  \n"      // 1
     "4 1 6.0  -102 \n"       // 2
     "4 1 9.0  -103 \n"       // 3
     "4 0 12.0 -104 \n"       // 4
     "4 0 15.0 -105 \n"       // 5
     "8 9 95.0 -100 \n"       // 6
     "9 4 9.0  -100 \n"       // 7
     "9 4 9.0  -99  \n",l4s); // 8

  // check stable sort
  BOOST_CHECK(ts.d_records.size() == stableSorted.d_records.size());
  for(size_t i=0; i < ts.d_records.size(); ++i)
   BOOST_CHECK(ts.d_records[i]== stableSorted.d_records[i]);

  // some find tests
  ts.d_keyCol=2;
  ts.d_resultCol=1;
  double r;
  r=-2;
  BOOST_CHECK( ts.interpolate(r,4.5));
  BOOST_CHECK( r==0.5); r=-2;
  BOOST_CHECK( ts.interpolate(r,(7)));
  BOOST_CHECK( r==1);   r=-2;
  BOOST_CHECK(!ts.interpolate(r,(-999999999)));
  BOOST_CHECK(!ts.interpolate(r,(999999999)));

  BOOST_CHECK( ts.interpolate(r,(14)));
  BOOST_CHECK( r==0);   r=-2;

  BOOST_CHECK( ts.interpolate(r,(1)));
  BOOST_CHECK( r==0);   r=-2;

  // none found prefix key empty
  ts.d_prefixKeyValue = 10;
  BOOST_CHECK(!ts.interpolate(r,4.5));
  BOOST_CHECK( r==-2); r=-2;
 }
 {
  double r=-2;
  MWFTest ts(
   "2  0 0  \n"
   "2  1 20 \n"
   "4  0 0  \n"
   "4  1 40 \n"
   "1  0 0  \n"
   "1  1 10 \n", 3);
  ts.d_prefixKeyValue = 1;
  BOOST_CHECK( ts.interpolate(r,0.5));
  BOOST_CHECK( r==5); r=-2;
  ts.d_prefixKeyValue = 2;
  BOOST_CHECK( ts.interpolate(r,0.5));
  BOOST_CHECK( r==10); r=-2;
  ts.d_prefixKeyValue = 4;
  BOOST_CHECK( ts.interpolate(r,0.5));
  BOOST_CHECK( r==20); r=-2;
  BOOST_CHECK(!ts.interpolate(r,-0.0001));
  BOOST_CHECK( r==-2);

  ts.d_prefixKeyValue = -1;
  BOOST_CHECK(!ts.interpolate(r,0.5));
  BOOST_CHECK( r==-2);

  ts.d_prefixKeyValue = 100;
  BOOST_CHECK(!ts.interpolate(r,0.5));
  BOOST_CHECK( r==-2);

 }
}

BOOST_AUTO_TEST_SUITE_END()
