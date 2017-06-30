#define BOOST_TEST_MODULE pcraster model_engine lddgraph
#include <boost/test/unit_test.hpp>
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "com_csfcell.h"
#include "calc_maskpacking.h"
#include "calc_asispacking.h"


#define private public
#define protected public
#include "calc_lddgraph.h"
#include "calc_downstreamvisitor.h"
#include "calc_TimeSliceVisitor.h"



// NOTE use string failureExpected in files expected to fail, see style guide



BOOST_AUTO_TEST_CASE(testSimple)
{
  using namespace calc;

  geo::RasterDim rd_aip(1,3);
  geo::RasterDim rd_mp(2,3);
  bool maskInit[6] = {false,false,false,true,true,true};
  std::vector<bool> mask(6);
  for(size_t i=0; i < 6; ++i)
    mask[i]=maskInit[i];
  AsIsPacking aip(rd_aip);
  MaskPacking mp(rd_mp,mask);
  IFieldRDConversion *conv[2]={&aip,&mp};

  UINT1 lddField[3]={6,5,4}; // -> P <-

  for (size_t i=0; i<2; ++i) {
    LddGraph ld(lddField,*conv[i]);

    BOOST_CHECK(ld.d_catchments.size()==1);
    BOOST_CHECK(ld.d_catchments[0].d_pitId    ==1);

    BOOST_CHECK(ld.d_edge.size()==2);
    BOOST_CHECK(ld.d_edge[1].source()==2);
    BOOST_CHECK(ld.d_edge[1].target()==1);
    BOOST_CHECK(ld.d_edge[0].source()==0);
    BOOST_CHECK(ld.d_edge[0].target()==1);
  }
}

BOOST_AUTO_TEST_CASE(testUpstream)
{
  using namespace calc;

  geo::RasterDim rd(2,4);
  AsIsPacking aip(rd);
  UINT1 lddField[8]={6,5,4,4,   // -> P <-<-
                     6,6,6,8 }; // -> ->->|^
  REAL4  resultR[8]={1,0,1,2,
                     6,5,4,3 };
  // init to something detectable
  REAL4  resultC[8]={9,9,9,9,
                     9,9,9,9 };
  LddGraph ld(lddField,aip);

  BOOST_CHECK(ld.d_edge.size()==7);
  BOOST_CHECK(ld.d_catchments.size()==1);

  resultC[ld.d_catchments[0].d_pitId]=0;
  for(LddGraph::UpConstIterator i=ld.upBegin();i!=ld.upEnd();++i)
    resultC[i->up()] = resultC[i->down()]+1;
  BOOST_CHECK(std::equal(resultC,resultC+8,resultR));
}

BOOST_AUTO_TEST_CASE(testDownstreamIterator)
{
  using namespace calc;

  geo::RasterDim rd(2,4);
  AsIsPacking aip(rd);
  UINT1 lddField[8]={6,5,4,4,   // -> X <-<-
                     6,6,6,8 }; // -> ->->|^
  // total of upstream edges as a
  // downstream traversal
  REAL4  resultR[8]={0,7,5,4,
                     0,1,2,3 };
  REAL4  resultC[8]={0,0,0,0,
                     0,0,0,0 };
  LddGraph ld(lddField,aip);

  BOOST_CHECK(ld.d_edge.size()==7);
  BOOST_CHECK(ld.d_catchments.size()==1);

  resultC[ld.d_catchments[0].d_pitId]=0;
  for(LddGraph::DownConstIterator i=ld.downBegin();i!=ld.downEnd();++i)
    resultC[i->down()] += resultC[i->up()]+1;
  BOOST_CHECK(std::equal(resultC,resultC+8,resultR));
}

//! \todo should go into RasterGraphTest
BOOST_AUTO_TEST_CASE(testInitField)
{
  using namespace calc;

  geo::RasterDim rd(2,4);
  AsIsPacking aip(rd);
  UINT1 lddField[8]={       6,5,4,4,   // -> X <-<-
                     MV_UINT1,6,6,8 }; // .  ->->|^
  REAL4  result[8]={1,1,1,1,
                    1,1,1,1 };

  LddGraph ld(lddField,aip);

  ld.initField<REAL4>(result,4.0);
  BOOST_CHECK(result[0]==4);
  BOOST_CHECK(result[3]==4);
  BOOST_CHECK(result[6]==4);
  BOOST_CHECK(pcr::isMV(result[4]));
}

BOOST_AUTO_TEST_CASE(testFieldIdToPitId)
{
/*
  {
    geo::RasterDim rd(2,4);
    AsIsPacking aip(rd);
    UINT1 lddField[8]={6,6,5,4,   // ->-> X<-
                       9,6,6,7 }; // -/ ->->\-
    // total of upstream edges as a
    // downstream traversal
    LddGraph ld(lddField,aip);

    BOOST_CHECK(ld.d_edge.size()        ==7);
    BOOST_CHECK(ld.d_catchments.size() ==1);
    BOOST_CHECK(ld.d_catchments[0].d_pitId=2);

    for(size_t i =0; i < 8; ++i)
      BOOST_CHECK(ld.d_fieldIdToPitId[i] == 0);
  }

  {
    geo::RasterDim rd(2,4);
    AsIsPacking aip(rd);
    UINT1 lddField[8]={MV_UINT1,6,5,4,   // ->-> X<-
                       6,5,6,7 }; // - X->->\-
    // total of upstream edges as a
    // downstream traversal
    LddGraph ld(lddField,aip);

    BOOST_CHECK(ld.d_edge.size()          ==5);
    BOOST_CHECK(ld.d_catchments.size()    ==2);
    BOOST_CHECK(ld.d_catchments[0].d_pitId ==2);
    BOOST_CHECK(ld.d_catchments[1].d_pitId ==5);

    BOOST_CHECK(ld.d_fieldIdToPitId(0) >= 2);
    BOOST_CHECK(ld.d_fieldIdToPitId(1) == 0);
    BOOST_CHECK(ld.d_fieldIdToPitId(2) == 0);
    BOOST_CHECK(ld.d_fieldIdToPitId(3) == 0);
    BOOST_CHECK(ld.d_fieldIdToPitId(4) == 1);
    BOOST_CHECK(ld.d_fieldIdToPitId(5) == 1);
    BOOST_CHECK(ld.d_fieldIdToPitId(6) == 0);
    BOOST_CHECK(ld.d_fieldIdToPitId(7) == 0);
  }
*/
}

BOOST_AUTO_TEST_CASE(testDownstream)
{
  using namespace calc;

  geo::RasterDim rd(2,4);
  AsIsPacking aip(rd);
  UINT1 lddField[8]={3,5,4,4,   // \  X <-<-
                     9,6,6,7 }; // -/ ->->\-
                                // 0, 1, 2, 3,
                                // 4, 5, 6, 7
  LddGraph ld(lddField,aip);
  BOOST_CHECK(ld.d_edge.size()==7);
  BOOST_CHECK(ld.d_catchments.size()==1);
}

BOOST_AUTO_TEST_CASE(testUnsound)
{
  using namespace calc;

  geo::RasterDim rd_aip(1,4);
  AsIsPacking aip(rd_aip);
  UINT1 lddField[4]={6,5,6,4}; // -> P -><-
  bool catched=false;
  try {
    LddGraph ld(lddField,aip);
  } catch(const LddGraph::Unsound& ) {
    catched=true;
  }
  BOOST_CHECK(catched);
}

namespace calc {
  struct DownstreamVisitorTester : public DownstreamVisitor {
    size_t d_vertexCount;
    size_t d_edgeCount;
    size_t d_pitIdOfCurrentCatchment;
    std::vector<size_t> d_pitOfMyCatchment;
    std::vector<size_t> d_v;
    std::vector<size_t> d_up;
    std::vector<size_t> d_down;

    DownstreamVisitorTester(LddGraph const& lg):
      DownstreamVisitor(lg),
      d_vertexCount(0),
      d_edgeCount(0),
      d_pitIdOfCurrentCatchment(10), // 10 as not visited marker
      d_pitOfMyCatchment(9,10)       // 10 as not visited marker
     {
        // the anticipated order of visitation
        d_v.push_back(1);
        d_up.push_back(1);
        d_down.push_back(3);
        d_v.push_back(3); //pit
        d_v.push_back(2);
        d_up.push_back(2);
        d_down.push_back(4);
        d_v.push_back(4);
        d_up.push_back(4);
        d_down.push_back(6);
        d_v.push_back(6); //pit
        d_v.push_back(5);
        d_up.push_back(5);
        d_down.push_back(7);
        d_v.push_back(7); //pit
        d_v.push_back(8); //pit
     }

    void finishVertex(size_t v) {
       BOOST_CHECK(d_v.size() > d_vertexCount);
       BOOST_CHECK(d_v[d_vertexCount]==v);
       d_vertexCount++;

       // must be set
       BOOST_CHECK(d_pitIdOfCurrentCatchment != 10);

       // set only once
       BOOST_CHECK(d_pitOfMyCatchment[v]     == 10);

       // set
       d_pitOfMyCatchment[v] = d_pitIdOfCurrentCatchment;
    }
    void visitEdge(size_t up, size_t down) {
       BOOST_CHECK(d_up.size() > d_edgeCount);
       BOOST_CHECK(d_up[d_edgeCount]  ==up);
       BOOST_CHECK(d_down.size() > d_edgeCount);
       BOOST_CHECK(d_down[d_edgeCount]==down);
       d_edgeCount++;
    }
    void startCatchment(size_t pitId)
    {
     d_pitIdOfCurrentCatchment  = pitId;
    }
  };
}



#if _MSC_VER
#ifdef DEBUG_DEVELOP
   // Bugzilla 178


BOOST_AUTO_TEST_CASE(testMVCtor)
{
  using namespace calc;

  geo::RasterDim rd(2,4);
  AsIsPacking aip(rd);
                  //        0,1,2,3
  UINT1 lddField[8]={       6,6,5,4,   // -> X <-<-
                     MV_UINT1,6,6,8 }; // .  ->->|^
                  //        4,5,6,7
  LddGraph in(lddField,aip);
  BOOST_CHECK(in.d_catchments.size()==1);
  BOOST_CHECK(in.d_catchments[0].d_pitId    ==2);

  BOOST_CHECK(in.d_edge.size()==6);
  // upstream order, breadth first
  BOOST_CHECK(in.d_edge[0].source()==1);
  BOOST_CHECK(in.d_edge[0].target()==2);
  BOOST_CHECK(in.d_edge[1].source()==3);
  BOOST_CHECK(in.d_edge[1].target()==2);
  BOOST_CHECK(in.d_edge[4].source()==5);
  BOOST_CHECK(in.d_edge[4].target()==6);
  BOOST_CHECK(in.d_edge[5].source()==0);
  BOOST_CHECK(in.d_edge[5].target()==1);

  BOOST_CHECK(in.d_mv.size()==1);
  BOOST_CHECK(in.d_mv[0]==4);

  {
    boost::dynamic_bitset<> isMV(8);
    // all init to 0
    BOOST_CHECK(isMV.none());
    isMV[5]=1;
    LddGraph l(in,isMV,true);

    BOOST_CHECK(l.d_edge.size()==2);
    // upstream order
    BOOST_CHECK(l.d_edge[0].source()==1);
    BOOST_CHECK(l.d_edge[0].target()==2);
    BOOST_CHECK(l.d_edge[1].source()==0);
    BOOST_CHECK(l.d_edge[1].target()==1);

    BOOST_CHECK(l.d_mv.size()==5);
  }
  { // remove only the pit
    boost::dynamic_bitset<> isMV(8);
    isMV[2]=1; // the pit

    LddGraph l1(in,isMV,true);

    // - minus the 2 edges to the pit
    BOOST_CHECK(l1.d_edge.size()==in.d_edge.size()-2);
    // catchment with invalid pidId
    BOOST_CHECK(!l1.d_catchments.empty());
    BOOST_CHECK(l1.invalid(l1.d_catchments[0].d_pitId));
    BOOST_CHECK(l1.d_mv.size()==2);
    BOOST_CHECK(l1.d_mv[0]==4);
    BOOST_CHECK(l1.d_mv[1]==2);

    LddGraph l2(l1,isMV,true);
    // - minus the 2 edges to the pit
    BOOST_CHECK(l2.d_edge.size()==in.d_edge.size()-2);
    // catchment with invalid pidId
    BOOST_CHECK(!l2.d_catchments.empty());
    BOOST_CHECK(l2.invalid(l2.d_catchments[0].d_pitId));
    BOOST_CHECK(l2.d_mv.size()==2);
    BOOST_CHECK(l2.d_mv[0]==4);
    BOOST_CHECK(l2.d_mv[1]==2);
  }
  { // remove all
    boost::dynamic_bitset<> isMV(8);
    isMV.flip();

    LddGraph l1(in,isMV,true);

    BOOST_CHECK(l1.d_catchments.empty());
    BOOST_CHECK(l1.d_edge.empty());
    BOOST_CHECK(l1.d_mv.size()==8);
  }
}

BOOST_AUTO_TEST_CASE(testVisitor)
{
  using namespace calc;

  geo::RasterDim rd_aip(3,3);
  AsIsPacking aip(rd_aip);
  UINT1 lddField[9]={
MV_UINT1,1,1,  // 0,1,2
       5,1,1,  // 3,4,5
       5,5,5   // 6,7,8
  };


  LddGraph ld(lddField,aip);

  BOOST_CHECK(ld.d_catchments.size()==4);
  BOOST_CHECK(ld.d_catchments[0].d_pitId==3);
  BOOST_CHECK(ld.d_catchments[0].d_beginEdge->up()==1);
  BOOST_CHECK(ld.d_catchments[0].d_beginEdge->down()==3);
  BOOST_CHECK(ld.d_catchments[1].d_pitId==6);
  BOOST_CHECK(ld.d_catchments[1].d_beginEdge->up()==4);
  BOOST_CHECK(ld.d_catchments[1].d_beginEdge->down()==6);
  BOOST_CHECK((ld.d_catchments[1].d_endEdge-1)->up()==2);
  BOOST_CHECK((ld.d_catchments[1].d_endEdge-1)->down()==4);

  {
    DownstreamVisitorTester dsv(ld);
    dsv.visitEntireLdd();
    size_t pitOfMyCatchment[9]={
        10,3,6,  // 0,1,2
         3,6,7,  // 3,4,5
         6,7,8   // 6,7,8
    };
    std::vector<size_t> expectV(pitOfMyCatchment,pitOfMyCatchment+9);
    BOOST_CHECK(dsv.d_pitOfMyCatchment == expectV);
  }

  boost::dynamic_bitset<> isMV(9);
  /*
       M,1,1,  // 0,1,0
       5,1,1,  // 0,0,0
       5,5,5   // 1,1,1
  */
  isMV[1]=1;
  isMV[6]=1;
  isMV[7]=1;
  isMV[8]=1;

  LddGraph ldMasked(ld,isMV,true);

  BOOST_CHECK(ldMasked.d_catchments.size()==2);

  // only edge deleted, pit intact
  BOOST_CHECK(ldMasked.d_catchments[0].d_pitId==3);
#if !defined(NDEBUG) && defined(_MSC_VER)
  // In debug builds, the VS STL validates iterators. The LddGraph code
  // juggles with containers and iterators. So much that VS's STL thinks
  // that iterators are invalidated. Because of some reserve() calls at the
  // right points in the code, they aren't, but only we know that.
  // Not sure what to test instead.
#else
  BOOST_CHECK(ldMasked.d_catchments[0].d_beginEdge==
            ldMasked.d_catchments[0].d_endEdge);
#endif

  // pit deleted + plus edge to pit deleted
  // only one edge remaining
  BOOST_CHECK(ldMasked.d_catchments[1].d_pitId==9);
  BOOST_CHECK(ldMasked.invalid(ldMasked.d_catchments[1].d_pitId));
  BOOST_CHECK((ldMasked.d_catchments[1].d_endEdge-
            ldMasked.d_catchments[1].d_beginEdge)==1);
  BOOST_CHECK(ldMasked.d_catchments[1].d_beginEdge->up()==2);
  BOOST_CHECK(ldMasked.d_catchments[1].d_beginEdge->down()==4);

/*
  // pit invalid, the single edge kep
  BOOST_CHECK(ldMasked.d_catchments[2].d_pitId==9);
  BOOST_CHECK(ldMasked.invalid(ldMasked.d_catchments[2].d_pitId));
  BOOST_CHECK((ldMasked.d_catchments[2].d_endEdge==
            ldMasked.d_catchments[2].d_beginEdge));
*/
}

BOOST_AUTO_TEST_CASE(testTimeSliceVisitor)
{
  using namespace calc;

  geo::RasterDim rd_aip(3,3);
  AsIsPacking aip(rd_aip);
#define FIELD_SIZE 9
  // 3 catchments
  UINT1 lddField[FIELD_SIZE]={
//                id's   catchment-id's
MV_UINT1,2,1,  // 0,1,2  X 0 0
MV_UINT1,1,1,  // 3,4,5  X 0 1
       5,5,5   // 6,7,8  0 1 2
  };

  Spatial nrTimeSlicesSpatial(VS_N, CRI_4, FIELD_SIZE);
  INT4  *nrTimeSlicesArray = (INT4 *)nrTimeSlicesSpatial.dest();
  for(size_t i = 0; i < FIELD_SIZE; ++i) {
    INT4 v(0);
    switch(i) {
     case 6 : v = 1; break;
     case 7 : v = 2; break;
     case 8 : v = 4; break;
    }
    nrTimeSlicesArray[i] = v;
  }
  VField<INT4> nrTimeSlicesVField(nrTimeSlicesSpatial, FIELD_SIZE);

  NonSpatial timestepInSecs(VS_S, 128.0F);

  struct T : public TimeSliceVisitor {
   std::vector<REAL4>  catchmentSliceInSecs;
   std::vector<size_t> nrInitVertexCalls;
   std::vector<size_t> nrFinishVertexCalls;
   size_t             nrInitPerCatchmentSliceCalls;
   CurrentSliceInfo d_csi;
   void initPerCatchmentSlice(CurrentSliceInfo const& csi)
   {
     nrInitPerCatchmentSliceCalls++;
     d_csi = csi;
   }
   void initVertexBeforeSlice(size_t v)
   {
    catchmentSliceInSecs[v] = (float)d_csi.sliceInSecs;
    nrInitVertexCalls[v]++;
   }
   void finishVertex(size_t v)
   {
    catchmentSliceInSecs[v] = (float)d_csi.sliceInSecs;
    nrFinishVertexCalls[v]++;
   }
   void visitEdge(size_t /*up*/, size_t /*down*/) {
   }
   T(const LddGraph& ldd,
     VField<INT4> const&  nrTimeSlices,
     Field        const&  timestepInSecs):
     TimeSliceVisitor(ldd,nrTimeSlices, timestepInSecs),
     catchmentSliceInSecs(FIELD_SIZE, 0.0F),
     nrInitVertexCalls(FIELD_SIZE,0),
     nrFinishVertexCalls(FIELD_SIZE, 0),
     nrInitPerCatchmentSliceCalls(0)
     {
     }
  };

  LddGraph ldd(lddField,aip);
  T t(ldd, nrTimeSlicesVField, timestepInSecs);
  t.visitPerCachmentSlice();

  // 3 all catchments once
  // 1,2,4 per slice for each cachment
  BOOST_CHECK_EQUAL(t.nrInitPerCatchmentSliceCalls, ((size_t)(3+1+2+4)));
  {
    REAL4 expect[FIELD_SIZE]={
      //           id's     catchment-id
       0,128,128, // 0,1,2  X 0 0
       0,128,64,  // 3,4,5  X 0 1
      128,64,32}; // 6,7,8  0 1 2
    std::vector<REAL4> expectV(expect,expect+FIELD_SIZE);
    BOOST_CHECK(t.catchmentSliceInSecs == expectV);
  }
  {
    size_t expect[FIELD_SIZE]={
      //          id's   catchment-id
       0,1,1,  // 0,1,2  X 0 0
       0,1,2,  // 3,4,5  X 0 1
       1,2,4}; // 6,7,8  0 1 2
    std::vector<size_t> expectV(expect,expect+FIELD_SIZE);
    BOOST_CHECK(t.nrInitVertexCalls == expectV);
    BOOST_CHECK(t.nrFinishVertexCalls == expectV);
  }
#undef FIELD_SIZE
}


#endif //ifdef DEBUG_DEVELOP
#endif //if _MSC_VER == 1400
