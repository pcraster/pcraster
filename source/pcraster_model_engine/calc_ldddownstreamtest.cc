#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LDDDOWNSTREAMTEST
#include "calc_ldddownstreamtest.h"
#define INCLUDED_CALC_LDDDOWNSTREAMTEST
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
#ifndef INCLUDED_CALC_MASKPACKING
#include "calc_maskpacking.h"
#define INCLUDED_CALC_MASKPACKING
#endif
#ifndef INCLUDED_CALC_ASISPACKING
#include "calc_asispacking.h"
#define INCLUDED_CALC_ASISPACKING
#endif
#ifndef INCLUDED_CALC_LDDDOWNSTREAM
#include "calc_ldddownstream.h"
#define INCLUDED_CALC_LDDDOWNSTREAM
#endif


/*!
  \file
  This file contains the implementation of the LddDownstreamTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC LDDDOWNSTREAM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::LddDownstreamTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<LddDownstreamTest> instance(new LddDownstreamTest());

  suite->add(BOOST_CLASS_TEST_CASE(&LddDownstreamTest::testSimple, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LddDownstreamTest::testUpstream, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LddDownstreamTest::testDownstream, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LddDownstreamTest::testDiagonal, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&LddDownstreamTest::testUnsound, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF LDDDOWNSTREAM MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::LddDownstreamTest::LddDownstreamTest()
{
}



//! setUp
void calc::LddDownstreamTest::setUp()
{
}



//! tearDown
void calc::LddDownstreamTest::tearDown()
{
}


void calc::LddDownstreamTest::testSimple()
{
  geo::RasterDim rd_aip(1,3);
  geo::RasterDim rd_mp(2,3);
  UINT1 mask[6] = {0,0,0,1,1,1};
  AsIsPacking aip(rd_aip);
  MaskPacking mp(rd_mp,mask);
  IFieldRDConversion *conv[2]={&aip,&mp};

  UINT1 lddField[3]={6,5,4}; // -> P <-

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


#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

void calc::LddDownstreamTest::testUpstream()
{
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

void calc::LddDownstreamTest::testDownstream()
{
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

void calc::LddDownstreamTest::testDiagonal()
{
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


void calc::LddDownstreamTest::testUnsound()
{
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
