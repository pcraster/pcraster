#define BOOST_TEST_MODULE pcraster mldd out_edge_iterator
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "mldd_outedgeiterator.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace mldd;

 {
  OutEdgeIterator begin(Vertex(1,1),0,0);
  OutEdgeIterator end  (Vertex(1,1));
  size_t i=0;
  for(OutEdgeIterator oi=begin; oi!=end; ++oi) {
    i++;
  }
  BOOST_CHECK(!i);
 }
 {
    OutEdgeIterator begin(Vertex(1,1),(1<<0)|(1<<6),0);
    OutEdgeIterator end  (Vertex(1,1));
    size_t i=0;
    for(OutEdgeIterator oi=begin; oi!=end; ++oi) {
        Edge e=oi.edge();
        BOOST_CHECK(e.source()==Vertex(1,1));
        switch (i) {
          case 0: BOOST_CHECK(e.target()==Vertex(2,0)); break;
          case 1: BOOST_CHECK(e.target()==Vertex(0,1)); break;
        };
        i++;
    }
    BOOST_CHECK(i==2);
 }
 {
    OutEdgeIterator begin(Vertex(1,1),(1<<0)|(1<<1),0);
    OutEdgeIterator end  (Vertex(1,1));
    size_t i=0;
    for(OutEdgeIterator oi=begin; oi!=end; ++oi) {
        Edge e=oi.edge();
        BOOST_CHECK(e.source()==Vertex(1,1));
        switch (i) {
          case 0: BOOST_CHECK(e.target()==Vertex(2,0)); break;
          case 1: BOOST_CHECK(e.target()==Vertex(2,1)); break;
        };
        i++;
    }
    BOOST_CHECK(i==2);
 }
 { // any() must do the same
    OutEdgeIterator begin(Vertex(1,1),(1<<0)|(1<<1),0);
    size_t i=0;
    for(OutEdgeIterator oi=begin; oi.any(); ++oi) {
        Edge e=oi.edge();
        BOOST_CHECK(e.source()==Vertex(1,1));
        switch (i) {
          case 0: BOOST_CHECK(e.target()==Vertex(2,0)); break;
          case 1: BOOST_CHECK(e.target()==Vertex(2,1)); break;
        };
        i++;
    }
    BOOST_CHECK(i==2);
 }
 {
    OutEdgeIterator begin(Vertex(1,1),0xFF,0);
    OutEdgeIterator end  (Vertex(1,1));
    size_t i=0;
    OutEdgeIterator oi=begin;
    for(    ; oi != end /* oi.any() */; ++oi) {
        i++;
        Edge e=oi.edge();
        BOOST_CHECK(e.source()==Vertex(1,1));
    }
    BOOST_CHECK(i==8);
    // keep at past the end
    ++oi;
    BOOST_CHECK(oi==end);
    ++oi;
    BOOST_CHECK(oi==end);
 }
}
