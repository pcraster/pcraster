#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_POINTTEST
#include "geo_pointtest.h"
#define INCLUDED_GEO_POINTTEST
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

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the PointTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::PointTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PointTest> instance(new PointTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PointTest::testLayout, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PointTest::testIndexDirection, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PointTest::testCloser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PointTest::testDistance, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF POINT MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::PointTest::PointTest()
{
}



//! setUp
void geo::PointTest::setUp()
{
}

//! tearDown
void geo::PointTest::tearDown()
{
}

void geo::PointTest::testLayout()
{
  // assure size of point is equal to
  // the vector of coordinates
 {
  typedef Point<float,2> Punt;
  BOOST_CHECK(sizeof(Punt)==8);
 }
 {
  typedef Point<double,3> Punt;
  BOOST_CHECK(sizeof(Punt)==24);
 }
}


void geo::PointTest::testIndexDirection()
{
 {
  //  1D:
  //          0
  //    <  1 -c- >= 0
  typedef Point<float,1> Punt;
  Punt c,p;
  c[0]=0;

  p[0]=-1;
  BOOST_CHECK(p.indexDirection(c) == 1);
  p[0]=1;
  BOOST_CHECK(p.indexDirection(c) == 0);
  p[0]=0;
  BOOST_CHECK(p.indexDirection(c) == 0);
 }
 {
 /*  2D:
  *   NW 1(b) | NE 0(a)
  *           0(e)
  *  1(h)-----C---------0(g)
  *   SW 3(c) | SE 2(d)
  *           2(f)
  */
  typedef Point<float,2> Punt;
  Punt c,p;
  c[0]=c[1]=0;
  p[0]=p[1]=1;
  BOOST_CHECK(p.indexDirection(c) == 0); // a
  p[0]=-1;
  BOOST_CHECK(p.indexDirection(c) == 1); // b
  p[0]=p[1]=-1;
  BOOST_CHECK(p.indexDirection(c) == 3); // c
  p[0]=1;
  BOOST_CHECK(p.indexDirection(c) == 2); // d

  p[0]=0;p[1]=1;
  BOOST_CHECK(p.indexDirection(c) == 0); // e
  p[0]=0;p[1]=-1;
  BOOST_CHECK(p.indexDirection(c) == 2); // f
  p[0]=1;p[1]=0;
  BOOST_CHECK(p.indexDirection(c) == 0); // g
  p[0]=-1;p[1]=0;
  BOOST_CHECK(p.indexDirection(c) == 1); // h
 }
}

void geo::PointTest::testCloser()
{
  typedef Point<float,2> P;
  typedef Closer<P>      C;

  C c(P(-5,0));

  BOOST_CHECK( c(P(1,0),P(2,0)));
  BOOST_CHECK(!c(P(2,0),P(1,0)));

  std::vector<P> l;
  l.push_back(P(2,0));
  l.push_back(P(1,0));

  BOOST_CHECK(l[0][X]==2);
  BOOST_CHECK(l[1][X]==1);
  std::sort(l.begin(),l.end(), C(P(-5,0)));
  BOOST_CHECK(l[0][X]==1);
  BOOST_CHECK(l[1][X]==2);
  // reverse
  std::sort(l.begin(),l.end(), std::not2(c));
  BOOST_CHECK(l[0][X]==2);
  BOOST_CHECK(l[1][X]==1);
}

void geo::PointTest::testDistance()
{
  typedef Point<float,2> P;

  // test compilation of enum
  BOOST_CHECK(P::Dim == 2);

  P p1(2,20),p2(3,21);
  BOOST_CHECK(p1.squaredDistance(p2) == 2);
  double d=p1.distance(p2);
  BOOST_CHECK(d>1.4 && d<1.43); // sqrt(2)
}
