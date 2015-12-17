#define BOOST_TEST_MODULE pcraster mldd dag_raster
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "mldd_upstream.h"
#include "mldd_weightmap.h"
#define private public
#include "mldd_dagraster.h"


#define CASE1_NR  6

struct Fixture
{

    Fixture()
    {
      using namespace mldd;

      // more realistic RIVM-37 case 1
      d_case1 = new DagRaster(geo::RasterDim(3,2));
      d_case1->addFlowNB(0,0,2);
      d_case1->addFlowNB(0,1,2);
      d_case1->addFlowNB(0,1,1);
      d_case1->addFlowNB(1,0,2);
      d_case1->addFlowNB(1,1,1);
      d_case1->updateOrder();
    }


    ~Fixture()
    {
      delete d_case1;
      d_case1 = 0;
    }

    mldd::DagRaster *d_case1;

};


BOOST_FIXTURE_TEST_SUITE(dag_raster, Fixture)

BOOST_AUTO_TEST_CASE(update_order)
{
  using namespace mldd;

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


BOOST_AUTO_TEST_CASE(downstream_visit)
{
  using namespace mldd;
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


BOOST_AUTO_TEST_CASE(cylcle)
{
  using namespace mldd;

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

BOOST_AUTO_TEST_SUITE_END()
