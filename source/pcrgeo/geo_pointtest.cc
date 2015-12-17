#define BOOST_TEST_MODULE pcraster geo point
#include <boost/test/unit_test.hpp>
#include <algorithm>
#include "geo_point.h"


BOOST_AUTO_TEST_CASE(layout)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(index_direction)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(closer)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(distance)
{
  using namespace geo;

  typedef Point<float,2> P;

  // test compilation of enum
  BOOST_CHECK(P::Dim == 2);

  P p1(2,20),p2(3,21);
  BOOST_CHECK(p1.squaredDistance(p2) == 2);
  double d=p1.distance(p2);
  BOOST_CHECK(d>1.4 && d<1.43); // sqrt(2)
}
