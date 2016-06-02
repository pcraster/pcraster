#define BOOST_TEST_MODULE pcraster model_engine astpar
#include <boost/test/unit_test.hpp>
#include "calc_astpar.h"
#include "calc_parset.h"


BOOST_AUTO_TEST_CASE(testCompare)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testSet)
{
  using namespace calc;

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
