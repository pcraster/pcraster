#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_OUTEDGEITERATORTEST
#include "mldd_outedgeiteratortest.h"
#define INCLUDED_MLDD_OUTEDGEITERATORTEST
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
#ifndef INCLUDED_MLDD_OUTEDGEITERATOR
#include "mldd_outedgeiterator.h"
#define INCLUDED_MLDD_OUTEDGEITERATOR
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the OutEdgeIteratorTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC OUTEDGEITERATOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*mldd::OutEdgeIteratorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<OutEdgeIteratorTest> instance(new OutEdgeIteratorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&OutEdgeIteratorTest::testCtor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF OUTEDGEITERATOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
mldd::OutEdgeIteratorTest::OutEdgeIteratorTest()
{
}



//! setUp
void mldd::OutEdgeIteratorTest::setUp()
{
}

//! tearDown
void mldd::OutEdgeIteratorTest::tearDown()
{
}

void mldd::OutEdgeIteratorTest::testCtor()
{
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
