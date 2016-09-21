#define BOOST_TEST_MODULE pcraster old_calc lookup_table
#include <boost/test/unit_test.hpp>
#include "com_pathname.h"
#include "com_exception.h"
#include "com_file.h"
#include "calc_lookuptable.h"
#include "calc_vs.h"
#include "calc_calc.h"  // globalInit()


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


struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()=default;

};


BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(old_style_constructor)
{
    using namespace calc;

    std::vector<VS> inKeys(1, VS_S);
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

    // 2.4 not a nominal
    bool failure=false;
    try {
        LookupTable t(VS_N);
        createTestTable(t,"[3 , 5 ] 2.4", inKeys);
    }
    catch(com::Exception const& exception) {
        BOOST_CHECK(
            exception.messages().find("2.4") != std::string::npos &&
            exception.messages().find("nominal") != std::string::npos);
        failure=true;
    }
    BOOST_CHECK(failure);
}


BOOST_AUTO_TEST_CASE(all_intervals)
{
  using namespace calc;

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


BOOST_AUTO_TEST_CASE(multiple_keys)
{
  using namespace calc;

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


BOOST_AUTO_TEST_CASE(multiple_records)
{
  using namespace calc;

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
