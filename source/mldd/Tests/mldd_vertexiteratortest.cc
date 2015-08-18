#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_VERTEXITERATORTEST
#include "mldd_vertexiteratortest.h"
#define INCLUDED_MLDD_VERTEXITERATORTEST
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

// Module headers.
#ifndef INCLUDED_MLDD_VERTEXITERATOR
#include "mldd_vertexiterator.h"
#define INCLUDED_MLDD_VERTEXITERATOR
#endif
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif

/*!
  \file
  This file contains the implementation of the VertexIteratorTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VERTEXITERATOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*mldd::VertexIteratorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VertexIteratorTest> instance(new VertexIteratorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VertexIteratorTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VERTEXITERATOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
mldd::VertexIteratorTest::VertexIteratorTest(){
}



//! setUp
void mldd::VertexIteratorTest::setUp()
{
}



//! tearDown
void mldd::VertexIteratorTest::tearDown()
{
}



void mldd::VertexIteratorTest::test()
{
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
