#define BOOST_TEST_MODULE pcraster mldd vertex_iterator
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "mldd_vertexiterator.h"
#include "mldd_dagraster.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace mldd;

  geo::RasterDim rd(2,3);
  DagRaster g(rd);
  // initially empty graph; no cells set

  BOOST_CHECK(g.beginVertex() == g.endVertex());
  BOOST_CHECK(g.beginVertex() == rd.convert(6));

  {
    VertexIterator  begin(g,g.beginVertex());
    VertexIterator  end(g,g.endVertex());
    BOOST_CHECK(begin == end);

    int count=0;
    for(VertexIterator i=begin; i != end; ++i)
      count++;
    BOOST_CHECK(!count);
  }

  // add 2 cells
  g.addFlowNB(0,0, 6); // -> add vertices (0,0) and (0,1)
  g.addFlowNB(0,1, 6); // -> add vertices (0,1) and (0,2)
  // can not add pit: g.addFlowNB(0,2, 5);
  {
      VertexIterator  begin(g,g.beginVertex());
      VertexIterator  end(g,g.endVertex());
      BOOST_CHECK(begin != end);

      int count=0;
      VertexIterator i=begin;
      for(  ; i != end; ++i)
        count++;
      BOOST_CHECK(count==3);
      // this iterator will proceed past the end
      ++i;
      BOOST_CHECK(i!=end);
      ++i;
      BOOST_CHECK(i!=end);
  }
}
