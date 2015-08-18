#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_DAGRASTERTEST
#include "mldd_dagrastertest.h"
#define INCLUDED_MLDD_DAGRASTERTEST
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
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif
#ifndef INCLUDED_MLDD_UPSTREAM
#include "mldd_upstream.h"
#define INCLUDED_MLDD_UPSTREAM
#endif
#ifndef INCLUDED_MLDD_WEIGHTMAP
#include "mldd_weightmap.h"
#define INCLUDED_MLDD_WEIGHTMAP
#endif

/*!
  \file
  This file contains the implementation of the DagRasterTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DAGRASTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*mldd::DagRasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DagRasterTest> instance(new DagRasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DagRasterTest::testUpdateOrder, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DagRasterTest::testDownstreamVisit, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DagRasterTest::testCycle, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DAGRASTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
mldd::DagRasterTest::DagRasterTest()
{
# define CASE1_NR  6
  // more realistic RIVM-37 case 1
  d_case1 = new DagRaster(geo::RasterDim(3,2));
  d_case1->addFlowNB(0,0,2);
  d_case1->addFlowNB(0,1,2);
  d_case1->addFlowNB(0,1,1);
  d_case1->addFlowNB(1,0,2);
  d_case1->addFlowNB(1,1,1);
  d_case1->updateOrder();
}

mldd::DagRasterTest::~DagRasterTest()
{
  delete d_case1;
  d_case1 = 0;
}

void mldd::DagRasterTest::testUpdateOrder()
{
  { // None
    DagRaster dr(geo::RasterDim(1,2));
    dr.updateOrder();
    BOOST_CHECK(dr.nrVertices()==0);
    BOOST_CHECK(dr.nrEdges()==0);
    BOOST_CHECK(dr.d_rto.size()==0);
  }
  { // Cycle
    DagRaster dr(geo::RasterDim(1,2));
    dr.addFlowNB(0,0,6);
    dr.addFlowNB(0,1,4);
    BOOST_CHECK(dr.nrVertices()==2);
    BOOST_CHECK(dr.nrEdges()==2);
    bool cycle=false;
    try {
     dr.updateOrder();
    } catch (const NotADag& c) {
      cycle=true;
    }
    BOOST_CHECK(cycle);
  }
  { // One
    DagRaster dr(geo::RasterDim(1,2));
    // only one from (0,1) -> (0,0)
    dr.addFlowNB(0,0,4);
    dr.addFlowNB(0,1,4);
    dr.updateOrder();

    BOOST_CHECK(dr.nrVertices()==2);
    BOOST_CHECK(dr.nrEdges()==1);
    BOOST_CHECK(dr.d_rto.size()==2);
    // in reverse
    BOOST_CHECK(dr.d_rto[0] == geo::CellLoc(0,0));
    BOOST_CHECK(dr.d_rto[1] == geo::CellLoc(0,1));
  }
  { // one out of map,TODO
    DagRaster dr(geo::RasterDim(1,1));
    dr.addFlowNB(0,0,1);
    dr.updateOrder();
    BOOST_CHECK(dr.nrVertices()==0);
    BOOST_CHECK(dr.nrEdges()==0);
    BOOST_CHECK(dr.d_rto.size()==0);
  }
  { // 2 different directions/OUTSIDE TODO
    DagRaster dr(geo::RasterDim(1,2));
    dr.addFlowNB(0,0,1);
    dr.addFlowNB(0,1,9);
    dr.updateOrder();
    BOOST_CHECK(dr.nrVertices()==0);
    BOOST_CHECK(dr.nrEdges()==0);
    BOOST_CHECK(dr.d_rto.size()==0);
  }
}

void mldd::DagRasterTest::testDownstreamVisit()
{
/*
  // DefineGraph is the most simple fo on here
  { // one out of map
    DagRaster dr(geo::RasterDim(1,1));
    UINT1 d[1]={0};
    dr.addFlowNB(0,0,1);
    DefineGraph dg(dr.rasterDim(),d);
    dr.downstreamVisitor(dg);
    BOOST_CHECK(d[0]==1);
  }
  { // Has begin vertex, but ends in Cycle
    DagRaster dr(geo::RasterDim(3,1));
    dr.addFlowNB(0,0,2);
    dr.addFlowNB(1,0,2);
    dr.addFlowNB(2,0,8); // begin vertex
    BOOST_CHECK(dr.nrVertices()==3);
    GraphVisit gv;
    // this does not catch it
    BOOST_CHECK(dr.sourceVertices().size() == 1);
    bool cycle=false;
    try {
      // this should catch it
      dr.downstreamVisitor(gv);
    } catch (const NotADag& c) {
      cycle=true;
    }
    // TODO BOOST_CHECK(cycle);
  }
  {
    DagRaster& dr(*d_case1);
    UINT1 d[6]={0,0,0,0,0,0};
    DefineGraph dg(dr.rasterDim(),d);
    std::cout << "nv " << dr.nrVertices() << "\n";
    BOOST_CHECK(dr.nrVertices()==5);
    // dr.downstreamVisitor(dg);
    for(size_t i=0; i < 5; i++) {
      std::cout << "i " << i << "\n";
      BOOST_CHECK(d[i]==1);
    }
    BOOST_CHECK(d[5]==MV_UINT1);
  }
  {
    DagRaster& dr(*d_case1);
    REAL4 d[6];
    Upstream u(dr.rasterDim(),d);
    dr.downstreamVisitor(u);
    BOOST_CHECK(d[0] == 0);
    BOOST_CHECK(d[1] == 0);
    std::cout << "d-2 " << d[2] << "\n";
    BOOST_CHECK(d[2] == 2);
    BOOST_CHECK(d[3] == 1);
    BOOST_CHECK(d[4] == 5);
    for(size_t i=0; i < 5; i++)
      BOOST_CHECK(d[i]==0);
    BOOST_CHECK(pcr::isMV(d[5]));
  }
*/
}


void mldd::DagRasterTest::testCycle()
{
  // DefineGraph is the most simple fo on here
  /*{ // one out of map
    DagRaster dr(geo::RasterDim(1,1));
    UINT1 d[1]={0};
    dr.addFlowNB(0,0,1);
    DefineGraph dg(dr.rasterDim(),d);
    dr.downstreamVisitor(dg);
    BOOST_CHECK(d[0]==1);
  } */
  { // Has begin vertex, but ends in Cycle
    DagRaster dr(geo::RasterDim(3,1));
    dr.addFlowNB(0,0,2);
    dr.addFlowNB(1,0,2);
    dr.addFlowNB(2,0,8); // begin vertex
    BOOST_CHECK(dr.nrVertices()==3);
    bool cycle=false;
    try {
      dr.updateOrder();
      // this should catch it
    } catch (const NotADag& c) {
      cycle=true;
    }
    BOOST_CHECK(cycle);
  }
  {
    DagRaster& dr(*d_case1);
    REAL4 out[CASE1_NR];
    // flat dem 1 every where
    geo::ScalarSimpleRaster dem(dr.rasterDim(),1);
    // upstream input is also 1
    // trick: use values of dem:
    REAL4  *in=&(dem[0]);
    WeightMap wm(dr,dem);
    Upstream u(wm,in,out);
    dr.downstreamVisitor(u);
    BOOST_CHECK(out[0] == 0);
    BOOST_CHECK(out[1] == 0);
    BOOST_CHECK(out[2] == 1.5);
    BOOST_CHECK(out[3] == 0.5);
    BOOST_CHECK(out[4] == 2);
    BOOST_CHECK(pcr::isMV(out[5]));
  }
}
