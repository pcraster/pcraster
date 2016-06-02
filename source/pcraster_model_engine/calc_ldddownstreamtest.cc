#define BOOST_TEST_MODULE pcraster model_engine ldddownstream
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "calc_asispacking.h"
#include "calc_maskpacking.h"

#define private public
#include "calc_ldddownstream.h"



BOOST_AUTO_TEST_CASE(testSimple)
{
  using namespace calc;

  geo::RasterDim rd_aip(1,3);
  geo::RasterDim rd_mp(2,3);
  // UINT1 mask[6] = {0,0,0,1,1,1};
  std::vector<bool> mask = {0,0,0,1,1,1};
  AsIsPacking aip(rd_aip);
  MaskPacking mp(rd_mp,mask);
  IFieldRDConversion *conv[2]={&aip,&mp};

  const UINT1 lddField[3]={6,5,4}; // -> P <-

  for (size_t i=0; i<2; ++i) {
    LddDownstream ld(lddField,*conv[i]);

    BOOST_CHECK(ld.d_pit.size()==1);
    BOOST_CHECK(ld.d_pit[0]    ==1);

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
  UINT1 lddField[8]={6,5,4,4,   // -> X <-<-
                     6,6,6,8 }; // -> ->->|^
  REAL4  resultR[8]={1,0,1,2,
                     6,5,4,3 };
  // init to something detectable
  REAL4  resultC[8]={9,9,9,9,
                     9,9,9,9 };
  LddDownstream ld(lddField,aip);

  BOOST_CHECK(ld.d_edge.size()==7);
  BOOST_CHECK(ld.d_pit.size()==1);

  resultC[ld.d_pit[0]]=0;
  for(LddDownstream::upIterator i=ld.upBegin();i!=ld.upEnd();++i)
    resultC[i->up()] = resultC[i->down()]+1;
  BOOST_CHECK(std::equal(resultC,resultC+8,resultR));
}

BOOST_AUTO_TEST_CASE(testDownstream)
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
  LddDownstream ld(lddField,aip);

  BOOST_CHECK(ld.d_edge.size()==7);
  BOOST_CHECK(ld.d_pit.size()==1);

  resultC[ld.d_pit[0]]=0;
  for(LddDownstream::downIterator i=ld.downBegin();i!=ld.downEnd();++i)
    resultC[i->down()] += resultC[i->up()]+1;
  BOOST_CHECK(std::equal(resultC,resultC+8,resultR));
}

BOOST_AUTO_TEST_CASE(testDiagonal)
{
  using namespace calc;

  geo::RasterDim rd(2,4);
  AsIsPacking aip(rd);
  UINT1 lddField[8]={6,5,4,4,   // -> X <-<-
                     9,6,6,7 }; // -/ ->->\-
  // total of upstream edges as a
  // downstream traversal
  LddDownstream ld(lddField,aip);

  BOOST_CHECK(ld.d_edge.size()==7);
  BOOST_CHECK(ld.d_pit.size()==1);
/*
 * TODO translate iterators to index
  resultC[ld.d_pit[0]]=0;
  for(LddDownstream::reverse_iterator i=ld.rbegin();i!=ld.rend();++i)
    resultC[i->down()] += resultC[i->up()]+1;
  BOOST_CHECK(std::equal(resultC,resultC+8,resultR));
*/
}

BOOST_AUTO_TEST_CASE(testUnsound)
{
  using namespace calc;

  geo::RasterDim rd_aip(1,4);
  AsIsPacking aip(rd_aip);
  UINT1 lddField[4]={6,5,6,4}; // -> P -><-
  bool catched=false;
  try {
    LddDownstream ld(lddField,aip);
  } catch(const LddDownstream::Unsound& u) {
    catched=true;
  }
  BOOST_CHECK(catched);
}
