#define BOOST_TEST_MODULE pcraster com clone
#include <boost/test/unit_test.hpp>
#include "com_intervalmap.h"
#include "com_intervaltypes.h"
#include "com_clone.h"


namespace com {
 namespace intervalMapTest {
   struct UnarySimplest : public std::unary_function<double,double>
   {
     double operator ()(double v) const
     {
          return v;
     }
   };
 }
}


namespace com {
 namespace intervalMapTest {
  struct FO_MM {
    std::vector<double> data;
    int  count;
    FO_MM(): count(0) {};
    void operator()(double v) {
        count++;
        data.push_back(v);
    }
  };
 }
}


BOOST_AUTO_TEST_CASE(find)
{
  using namespace com;

  typedef IntervalMap<int> M;
  M m;
  BOOST_CHECK(m.outside()==0);
  BOOST_CHECK(
   m.insertInterval(BetweenLimits<>(GreaterThan<>(3),LessThanEqualTo<>(4))));
  BOOST_CHECK(
   m.insertInterval(GreaterThan<>(4)));
  m.findValue(2)++;
  m.findValue(4)++;
  m.findValue(5)++;
  m.findValue(10)++;

  BOOST_CHECK(m.outside()==1);       // 2
  BOOST_CHECK(m.findValue(3)==1);    // equals outside
  BOOST_CHECK(m.findValue(3.5)==1);  // 4
  BOOST_CHECK(m.findValue(5)==2);    // 5,10


  // check order on intervals
  BOOST_CHECK(m.size()==2);
  m.insertInterval(BetweenLimits<>(GreaterThan<>(0),LessThanEqualTo<>(2)));
  BOOST_CHECK(m.size()==3);
  size_t c=0;
  for(M::iterator i=m.begin();i!=m.end();++i) {
    switch(c) {
      case 0: BOOST_CHECK(i->first->min()==0); break;
      case 1: BOOST_CHECK(i->first->min()==3); break;
      case 2: BOOST_CHECK(i->first->min()==4); break;
    };
    c++;
  }
}


BOOST_AUTO_TEST_CASE(visit)
{
  using namespace com;

  typedef IntervalMap<intervalMapTest::FO_MM> M;
  M m;
  m.insertInterval(BetweenLimits<>(GreaterThan<>(3),LessThanEqualTo<>(4)));
  m.insertInterval(GreaterThan<>(4));

  double val[] = {2,   // outside
                  10,  // <4, >
                  4,   // <3,4]
                  5 }; // <4, >
  for(size_t i=0; i < 4; ++i)
    m.visit(val[i]);

  BOOST_CHECK(m.outside().count==1);       // 2
  BOOST_CHECK(m.outside().data[0]==2);     // 2
  BOOST_CHECK(m.findValue(3).count==1);    // equals outside
  BOOST_CHECK(m.findValue(3).data[0]==2);  // equals outside
  BOOST_CHECK(m.findValue(3.5).count==1);  // 4
  BOOST_CHECK(m.findValue(3.5).data[0]==4);// equals outside
  BOOST_CHECK(m.findValue(5).count==2);    // 5,10
  BOOST_CHECK(m.findValue(5).data[0]==10);
  BOOST_CHECK(m.findValue(5).data[1]==5);

  // <3,4]
  double *p =
   m.partition(val,val+4,m.begin(), intervalMapTest::UnarySimplest());
  BOOST_CHECK(std::distance(val,p)==1);
  BOOST_CHECK(val[0]==4);

  // <4, >
  p =
   m.partition(val,val+4,++(m.begin()), intervalMapTest::UnarySimplest());
  BOOST_CHECK(std::distance(val,p)==2);
  BOOST_CHECK(val[0]+val[1] == 15);

  // outside
  p =
   m.partitionOutside(val,val+4,intervalMapTest::UnarySimplest());
  BOOST_CHECK(std::distance(val,p)==1);
  BOOST_CHECK(val[0] == 2);
}


BOOST_AUTO_TEST_CASE(overlap)
{
  using namespace com;

  IntervalMap<int> m;
  // typedef IntervalMap<int>::iterator I;
  BOOST_CHECK( m.insertInterval(GreaterThan<>(4)));
  BOOST_CHECK(!m.insertInterval(GreaterThan<>(5)));

  m.clear();
  BOOST_CHECK(m.insertInterval(GreaterThan<>(5)));

  m.clear();
  BOOST_CHECK(m.insertInterval(AnythingInterval<>()));
  BOOST_CHECK(!m.insertInterval(GreaterThan<>(5)));

  m.clear();
  BOOST_CHECK( m.insertInterval(EqualTo<>(5)));
  BOOST_CHECK(!m.insertInterval(GreaterThanEqualTo<>(5)));
  BOOST_CHECK( m.insertInterval(GreaterThan<>(5)));

}


BOOST_AUTO_TEST_CASE(multi_map)
{
  using namespace com;

  typedef IntervalMultiMap<intervalMapTest::FO_MM> M;
  M m;
  // typedef IntervalMap<int>::iterator I;

  // as long as implemented as vector this
  // is a very simple test to write;

  m.insertInterval(GreaterThan<>(4));
  m.insertInterval(BetweenLimits<>(GreaterThan<>(-3),LessThan<>(4.3)));
  m.insertInterval(GreaterThan<>(5));
  BOOST_CHECK(m.size()==3);
  m.insertInterval(BetweenLimits<>(GreaterThan<>(5),LessThan<>(6)));
  BOOST_CHECK(m.size()==4);

  double val[] = { 4.2, -2, -9 };
  m.visit(4.2);
  m.visit(-2);
  m.visit(-9);
  BOOST_CHECK(m[0].second.count==1);
  BOOST_CHECK(m[0].second.data[0]==4.2);
  BOOST_CHECK(m[1].second.count==2);
  BOOST_CHECK(m[1].second.data[0]==4.2);
  BOOST_CHECK(m[1].second.data[1]==-2);
  BOOST_CHECK(m[2].second.count==0);
  BOOST_CHECK(m[3].second.count==0);
  BOOST_CHECK(m.outside().count==1);
  BOOST_CHECK(m.outside().data[0]==-9);

  // > 4
  double *p =
   m.partition(val,val+3,m.begin(), intervalMapTest::UnarySimplest());
  BOOST_CHECK(std::distance(val,p)==1);
  BOOST_CHECK(val[0]==4.2);

  // <-3, 4.3 >
  p =
   m.partition(val,val+3,++(m.begin()), intervalMapTest::UnarySimplest());
  BOOST_CHECK(std::distance(val,p)==2);
  BOOST_CHECK(val[0]+val[1] == 2.2); // 4.2+-2

  // outside
  p =
   m.partitionOutside(val,val+3,intervalMapTest::UnarySimplest());
  BOOST_CHECK(std::distance(val,p)==1);
  BOOST_CHECK(val[0] == -9);

}


BOOST_AUTO_TEST_CASE(no_overlap)
{
  using namespace com;

  std::vector<const com::Interval<float> *> v;
  v.push_back(new com::LessThan<float>(4));
  v.push_back(new com::GreaterThan<float>(4));
  BOOST_CHECK( noOverlap<float>(v));
  v.push_back(new com::GreaterThan<float>(4.5));
  BOOST_CHECK(!noOverlap<float>(v));

  clearClone(v);
}
