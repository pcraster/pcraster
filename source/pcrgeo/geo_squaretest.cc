#define BOOST_TEST_MODULE pcraster geo square
#include <boost/test/unit_test.hpp>
#include "geo_square.h"


BOOST_AUTO_TEST_CASE(quad_square_at)
{
  using namespace geo;

  typedef Point<float,2> Punt;
  Punt c;
  c[0]=20; c[1]=20;
  typedef geo::Square<float,2> Kant;
  Kant s(c,10);
 /*  2D:
  *   NW 1(b) | NE 0(a)
  *           0(e)
  *  1(h)-----C---------0(g)
  *   SW 3(c) | SE 2(d)
  *           2(f)
  */
  Kant q;
  q = s.quadSquareAt(0);
  BOOST_CHECK(q.halfWidth()==5);
  c[0]=25; c[1]=25;
  BOOST_CHECK(q.centre()==c); // a

  q = s.quadSquareAt(1);
  BOOST_CHECK(q.halfWidth()==5);
  c[0]=15; c[1]=25;
  BOOST_CHECK(q.centre()==c); // b
  c[0]=15; c[1]=15;
  q = s.quadSquareAt(3);
  BOOST_CHECK(q.centre()==c); // c
  c[0]=25; c[1]=15;
  q = s.quadSquareAt(2);
  BOOST_CHECK(q.centre()==c); // d
}


BOOST_AUTO_TEST_CASE(contains)
{
  using namespace geo;

  typedef Point<float,2> Punt;
  Punt c; c[0]=20; c[1]=20;

  // default boundary, closed
  typedef geo::Square<float,2> Kant;
  Kant s(c,10);
  BOOST_CHECK(s.contains(c));

  Punt p;
  p[0]=12;p[1]=c[1];
  BOOST_CHECK(s.contains(p));

  p[0]=10; // on boundary
  BOOST_CHECK(s.contains(p));


  typedef geo::Square<float,2,OpenBoundaries> Open;
  Open open(c,10);
  // not the edge
  BOOST_CHECK(!open.contains(p));

  typedef geo::Square<float,2,ClosedBoundaries> Closed;
  Closed closed(c,10);
  // the edge is in
  BOOST_CHECK(closed.contains(p));

  typedef geo::Square<float,2,ClosedOpenBoundaries> ClosedOpen;
  ClosedOpen closedOpen(c,10);
  p[0]=10; // left/lower is closed
  BOOST_CHECK( closedOpen.contains(p));
  p[0]=30; // right/higher is open
  BOOST_CHECK(!closedOpen.contains(p));

  typedef geo::Square<float,2,OpenClosedBoundaries> OpenClosed;
  OpenClosed openClosed(c,10);
  p[0]=10; // left/lower is open
  BOOST_CHECK(!openClosed.contains(p));
  p[0]=30; // right/higher is closed
  BOOST_CHECK( openClosed.contains(p));

  {
   typedef geo::Square<float,2,ClosedOpenBoundaries> OpenClosed;
   OpenClosed oc(Punt(179020,330940),80);
   BOOST_CHECK(!oc.contains(Punt(179973,332255)));
  }
}


BOOST_AUTO_TEST_CASE(intersects)
{
  using namespace geo;

  typedef Point<float,2> Punt;
  Punt c(20,20);
  // default boundary, closed
  typedef geo::Square<float,2> Kant;
  Kant s(c,10);
  //! intersects with itself
  BOOST_CHECK(s.intersects(s));

  { // fully contained
   Kant is(c,5);
   BOOST_CHECK(is.intersects(s));
   BOOST_CHECK(s.intersects(is));
  }
  { // partial
    Kant is(Punt(15,15),8);
    BOOST_CHECK(is.intersects(s));
    BOOST_CHECK(s.intersects(is));
  }
  { // touch edge
    Kant is(Punt(5,5),5);
    BOOST_CHECK(is.intersects(s));
    BOOST_CHECK(s.intersects(is));
  }
  { 
	// no edge touch with open boundaries
    typedef geo::Square<float,2,OpenBoundaries> OK;
    OK os(c,10);
    OK is(Punt(5,5),5);
    BOOST_CHECK(!os.intersects(is));
    BOOST_CHECK(!is.intersects(os));
  }
  { // no edge on in each other, midpoints 
	// must be checked
	//
	Kant os(Punt(74,6),40);
	Kant is(Punt(0,0),60);
    BOOST_CHECK( os.intersects(is));
    BOOST_CHECK( is.intersects(os));
  }
  { // debug case
    Kant os(Punt(2,20),1);
    Kant is(Punt(0.78125f,21.0938f),0.78125f);
    BOOST_CHECK( os.intersects(is));
    BOOST_CHECK( is.intersects(os));
  }
}
