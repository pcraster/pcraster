#define BOOST_TEST_MODULE pcraster geo square
#include <boost/test/unit_test.hpp>
#include "geo_square.h"

BOOST_AUTO_TEST_CASE(quad_square_at)
{
  using namespace geo;

  using Punt = Point<float, 2>;
  Punt c;
  c[0] = 20;
  c[1] = 20;
  using Kant = geo::Square<float, 2>;
  Kant const s(c, 10);
  /*  2D:
  *   NW 1(b) | NE 0(a)
  *           0(e)
  *  1(h)-----C---------0(g)
  *   SW 3(c) | SE 2(d)
  *           2(f)
  */
  Kant q;
  q = s.quadSquareAt(0);
  BOOST_TEST(q.halfWidth() == 5);
  c[0] = 25;
  c[1] = 25;
  BOOST_TEST(q.centre() == c);  // a

  q = s.quadSquareAt(1);
  BOOST_TEST(q.halfWidth() == 5);
  c[0] = 15;
  c[1] = 25;
  BOOST_TEST(q.centre() == c);  // b
  c[0] = 15;
  c[1] = 15;
  q = s.quadSquareAt(3);
  BOOST_TEST(q.centre() == c);  // c
  c[0] = 25;
  c[1] = 15;
  q = s.quadSquareAt(2);
  BOOST_TEST(q.centre() == c);  // d
}

BOOST_AUTO_TEST_CASE(contains)
{
  using namespace geo;

  using Punt = Point<float, 2>;
  Punt c;
  c[0] = 20;
  c[1] = 20;

  // default boundary, closed
  using Kant = geo::Square<float, 2>;
  Kant const s(c, 10);
  BOOST_TEST(s.contains(c));

  Punt p;
  p[0] = 12;
  p[1] = c[1];
  BOOST_TEST(s.contains(p));

  p[0] = 10;  // on boundary
  BOOST_TEST(s.contains(p));


  using Open = geo::Square<float, 2, OpenBoundaries>;
  Open const open(c, 10);
  // not the edge
  BOOST_TEST(!open.contains(p));

  using Closed = geo::Square<float, 2, ClosedBoundaries>;
  Closed const closed(c, 10);
  // the edge is in
  BOOST_TEST(closed.contains(p));

  using ClosedOpen = geo::Square<float, 2, ClosedOpenBoundaries>;
  ClosedOpen const closedOpen(c, 10);
  p[0] = 10;  // left/lower is closed
  BOOST_TEST(closedOpen.contains(p));
  p[0] = 30;  // right/higher is open
  BOOST_TEST(!closedOpen.contains(p));

  using OpenClosed = geo::Square<float, 2, OpenClosedBoundaries>;
  OpenClosed const openClosed(c, 10);
  p[0] = 10;  // left/lower is open
  BOOST_TEST(!openClosed.contains(p));
  p[0] = 30;  // right/higher is closed
  BOOST_TEST(openClosed.contains(p));

  {
    using OpenClosed = geo::Square<float, 2, ClosedOpenBoundaries>;
    OpenClosed const oc(Punt(179020, 330940), 80);
    BOOST_TEST(!oc.contains(Punt(179973, 332255)));
  }
}

BOOST_AUTO_TEST_CASE(intersects)
{
  using namespace geo;

  using Punt = Point<float, 2>;
  Punt const c(20, 20);
  // default boundary, closed
  using Kant = geo::Square<float, 2>;
  Kant const s(c, 10);
  //! intersects with itself
  BOOST_TEST(s.intersects(s));

  {  // fully contained
    Kant const is(c, 5);
    BOOST_TEST(is.intersects(s));
    BOOST_TEST(s.intersects(is));
  }
  {  // partial
    Kant const is(Punt(15, 15), 8);
    BOOST_TEST(is.intersects(s));
    BOOST_TEST(s.intersects(is));
  }
  {  // touch edge
    Kant const is(Punt(5, 5), 5);
    BOOST_TEST(is.intersects(s));
    BOOST_TEST(s.intersects(is));
  }
  {
    // no edge touch with open boundaries
    using OK = geo::Square<float, 2, OpenBoundaries>;
    OK const os(c, 10);
    OK const is(Punt(5, 5), 5);
    BOOST_TEST(!os.intersects(is));
    BOOST_TEST(!is.intersects(os));
  }
  {  // no edge on in each other, midpoints
    // must be checked
    //
    Kant const os(Punt(74, 6), 40);
    Kant const is(Punt(0, 0), 60);
    BOOST_TEST(os.intersects(is));
    BOOST_TEST(is.intersects(os));
  }
  {  // debug case
    Kant const os(Punt(2, 20), 1);
    Kant const is(Punt(0.78125F, 21.0938F), 0.78125F);
    BOOST_TEST(os.intersects(is));
    BOOST_TEST(is.intersects(os));
  }
}
