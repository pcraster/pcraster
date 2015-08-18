#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_PRDIMTREETEST
#include "geom_prdimtreetest.h"
#define INCLUDED_GEOM_PRDIMTREETEST
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
#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif
#ifndef INCLUDED_COM_RLEPTRVECTOR
#include "com_rleptrvector.h"
#define INCLUDED_COM_RLEPTRVECTOR
#endif
// Module headers.

#ifndef INCLUDED_GEOM_PRDIMTREE
#include "geom_prdimtree.cc"
#define INCLUDED_GEOM_PRDIMTREE
#endif
#ifndef INCLUDED_GEOM_PROXIMITYSEARCH
#include "geom_proximitysearch.h"
#define INCLUDED_GEOM_PROXIMITYSEARCH
#endif



/*!
  \file
  This file contains the implementation of the PRDimTreeTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PRDIMTREE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geom::PRDimTreeTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PRDimTreeTest> instance(new PRDimTreeTest());
  suite->add(BOOST_CLASS_TEST_CASE(&PRDimTreeTest::testCompile, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PRDimTreeTest::testKeyElement, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PRDimTreeTest::testProximitySearch, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PRDimTreeTest::testElementIsKeyPtr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PRDimTreeTest::testRLEPtr, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PRDIMTREE MEMBERS
//------------------------------------------------------------------------------

//! ctor
geom::PRDimTreeTest::PRDimTreeTest()
{
}



//! setUp
void geom::PRDimTreeTest::setUp()
{
}

//! tearDown
void geom::PRDimTreeTest::tearDown()
{
}

void geom::PRDimTreeTest::testCompile()
{
  typedef geo::Point<float, 2> Punt;
  BOOST_CHECK(sizeof(Punt)==8);
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,Punt > > pr(c,50,3);
  Punt p;
  p[0]=2;
  for (size_t i=3; i<8; i++) {
   p[1]=i;
   pr.insert(p);
   BOOST_CHECK(pr.count(p)==1);
  }
  p[1]=7;
  pr.insert(p);
  BOOST_CHECK(pr.count(p)==2);

  // TODO outside, should throw range_error?
  p[1]=102;
  BOOST_CHECK(pr.count(p)==0);

  // insert some edge cases

  // the centre
  pr.insert(c);
  BOOST_CHECK(pr.count(c)==1);

  // the edge, is part
  p[0]=p[1]=0;
  pr.insert(p);
  BOOST_CHECK(pr.count(p)==1);

  p[0]=5;
  for (size_t i=0; i<8; i++) {
   p[1]=5;
   pr.insert(p);
  }
  BOOST_CHECK(pr.count(p)==8);
}

namespace geom {
 typedef geo::Point<float,2> Punt;
  struct TestPRDimElement1: public Punt {
    int                 value;
    const Punt& key() const {
      return *this;
    }
    TestPRDimElement1(const Punt& p):
      Punt(p) {};
    TestPRDimElement1() {};
  };
  struct TestPRDimElement2 {
    int                value;
    Punt               d_p;
    const Punt& key() const {
      return d_p;
    }
    TestPRDimElement2() {};
    TestPRDimElement2(const Punt& p):
      d_p(p) {};
  };
}

void geom::PRDimTreeTest::testKeyElement()
{
 { // Element is Key through inheritance, Default
  BOOST_CHECK(sizeof(TestPRDimElement1)==12);
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElement1> > pr(c,50,3);

  Punt p;
  p[0]=2;
  for (size_t i=3; i<8; i++) {
   p[1]=i;
   pr.insert(TestPRDimElement1(p));
   BOOST_CHECK(pr.count(p)==1);
  }
 }
 { // Element is Key through inheritance, Policy set
  BOOST_CHECK(sizeof(TestPRDimElement1)==12);
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElement1,ElementIsKey> > pr(c,50,3);

  Punt p;
  p[0]=2;
  for (size_t i=3; i<8; i++) {
   p[1]=i;
   pr.insert(TestPRDimElement1(p));
   BOOST_CHECK(pr.count(p)==1);
  }
 }
 { // Element is Key through inheritance, but also has key() method
  BOOST_CHECK(sizeof(TestPRDimElement1)==12);
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElement1,ElementHasKey> > pr(c,50,3);

  Punt p;
  p[0]=2;
  for (size_t i=3; i<8; i++) {
   p[1]=i;
   pr.insert(TestPRDimElement1(p));
   BOOST_CHECK(pr.count(p)==1);
  }
 }
 { // Element is not a Key, but has key() method
  BOOST_CHECK(sizeof(TestPRDimElement2)==12);
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElement2,ElementHasKey> > pr(c,50,3);

  Punt p;
  p[0]=2;
  for (size_t i=3; i<8; i++) {
   p[1]=i;
   pr.insert(TestPRDimElement2(p));
   BOOST_CHECK(pr.count(p)==1);
  }
  p[1]=3;
  pr.insert(TestPRDimElement2(p));
  BOOST_CHECK(pr.count(p)==2);
 }
}

void geom::PRDimTreeTest::testProximitySearch()
{
  BOOST_CHECK(sizeof(TestPRDimElement2)==12);
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElement2,ElementHasKey> > pr(c,50,3);

  Punt p;
  for(size_t j=1; j<=3; ++j)
  for (size_t i=10; i<30; ++i) {
   p[0]=j;
   p[1]=i;
   pr.insert(TestPRDimElement2(p));
   BOOST_CHECK(pr.count(p)==1);
  }

 // all
 std::vector<TestPRDimElement2> result;
 pr.proximitySearch(result,c,1000);
 BOOST_CHECK(result.size()==60);

 p[0]=2; p[1]=20;
 // circular search
 pr.proximitySearch(result,p,1);
 // 5 : centre + 4 wind-directions
 BOOST_CHECK(result.size()==5);

 // squared search
 ProximitySearch psSq(1.01);
 psSq.setSquared(true);
 pr.proximitySearch(result,p,psSq);
 // 9 : full square grid
 BOOST_CHECK(result.size()==9);
 for(size_t i=0; i < 9; ++i)
   BOOST_CHECK(p.distance(result[i].key()) < 1.43); // sqrt(2)

 psSq.setRadius(1); // leaf out on ClosedOpen
 pr.proximitySearch(result,p,psSq);
 BOOST_CHECK(result.size()==4);
 for(size_t i=0; i < 4; ++i) {
   BOOST_CHECK(p.distance(result[i].key()) < 1.43); // sqrt(2)
   BOOST_CHECK(p[0] >= result[i].key()[0]);
   BOOST_CHECK(p[1] >= result[i].key()[1]);
 }

 psSq.setRadius(1.01); // reset
 // search with maximum
 psSq.setMaxNr(5);
 pr.proximitySearch(result,p,psSq);
 BOOST_CHECK(result.size()==5);
 // 5 : centre + 4 wind-directions
 for(size_t i=0; i < 5; ++i)
   BOOST_CHECK(p.distance(result[i].key()) == 1 ||
             p == result[i].key());

 ProximitySearch psMin(0.5); // only centre
 pr.proximitySearch(result,p,psMin);
 BOOST_CHECK(result.size()==1);
 BOOST_CHECK(p == result[0].key());
 // search with minimum
 psMin.setMinNr(2);
 pr.proximitySearch(result,p,psMin);
 BOOST_CHECK(result.empty());

}


namespace geom {
  struct TestPRDimElemKeyIsPtr_Ptr {
    float              x,y,v;
    const Punt& key() const {
      return (const Punt&)*this;
    }
    TestPRDimElemKeyIsPtr_Ptr() {};
  };
  typedef const TestPRDimElemKeyIsPtr_Ptr* TestPRDimElemKeyIsPtr;
}

void geom::PRDimTreeTest::testElementIsKeyPtr()
{
#ifdef __x86_64__
  bool todo__x86_64__=false;
  BOOST_WARN(todo__x86_64__);
#endif
 BOOST_CHECK(sizeof(TestPRDimElemKeyIsPtr)==4);
 BOOST_CHECK(sizeof(TestPRDimElemKeyIsPtr_Ptr)==12);
 {
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElemKeyIsPtr,ElementIsKeyPtr> > 
    pr(c,50,3);

  typedef struct XYV { float x,y,v;} XYV;
  XYV p[3]= { { 3,6,9 }, { 8, 9, 10}, { 4,5,6}};
  for (size_t i=0; i<3; i++) {
   BOOST_CHECK(pr.insert((TestPRDimElemKeyIsPtr)p+i));
   Punt s(p[i].x,p[i].y);
   BOOST_CHECK(pr.count(s)==1);
  }
  XYV out= { 80000, 9, 10};
  BOOST_CHECK(!pr.insert((TestPRDimElemKeyIsPtr)&out));

  std::vector<TestPRDimElemKeyIsPtr> result;
  pr.proximitySearch(result,c,1000);
  BOOST_CHECK(result.size()==3);
 }
}

#define RLE_NR 3
void geom::PRDimTreeTest::testRLEPtr()
{
#ifdef __x86_64__
  bool RLEPtrVectorNotonx86_64=false;
  BOOST_WARN(RLEPtrVectorNotonx86_64);
#else
 BOOST_CHECK(sizeof(TestPRDimElemKeyIsPtr)==4);
 BOOST_CHECK(sizeof(TestPRDimElemKeyIsPtr_Ptr)==12);
 {
  Punt c;
  c[0]=50;
  c[1]=50;
  PRDimTree<PRDimT<float,2,TestPRDimElemKeyIsPtr,
        ElementIsKeyPtr,
        com::RLEPtrVector<TestPRDimElemKeyIsPtr_Ptr> > > pr(c,50,3);

  typedef struct XYV { float x,y,v;} XYV;
  XYV p[RLE_NR]= { { 3,6,9 }, { 8, 9, 10}, { 4,5,6} };
  for (size_t i=0; i<RLE_NR; i++) {
   BOOST_CHECK(pr.insert((TestPRDimElemKeyIsPtr)p+i));
   Punt s(p[i].x,p[i].y);
   BOOST_CHECK(pr.count(s)==1);
  }
  XYV out= { 80000, 9, 10};
  BOOST_CHECK(!pr.insert((TestPRDimElemKeyIsPtr)&out));

  std::vector<TestPRDimElemKeyIsPtr> result;
  pr.proximitySearch(result,c,1000);
  BOOST_CHECK(result.size()==3);
 }
#endif
}
