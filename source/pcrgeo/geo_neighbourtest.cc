#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_NEIGHBOURTEST
#include "geo_neighbourtest.h"
#define INCLUDED_GEO_NEIGHBOURTEST
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
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif


/*!
  \file
  This file contains the implementation of the NeighbourTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC NEIGHBOUR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::NeighbourTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<NeighbourTest> instance(new NeighbourTest());

  suite->add(BOOST_CLASS_TEST_CASE(&NeighbourTest::testLinearDownStream, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&NeighbourTest::testConversion, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&NeighbourTest::testNBCode, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&NeighbourTest::testDownStreamVisitorCell, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF NEIGHBOUR MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::NeighbourTest::NeighbourTest()
{
}



//! setUp
void geo::NeighbourTest::setUp()
{
}

//! tearDown
void geo::NeighbourTest::tearDown()
{
}

namespace geo {

static bool neighbourTarget(
    LinearLoc  s,
    const RasterDim&  rd,
    LDD::Code  l,
    bool       outside)
{
  LinearLoc d=LDD::target(s,l,rd.nrCells(),rd.nrCols());
  if (d>=rd.nrCells())
    return outside;
  CellLoc d2= LDD::target(rd.convert(s),l);
  return d2 == rd.convert(d);
}

static bool nbTarget(
    LinearLoc  s,
    const RasterDim&  rd,
    NB::Code    l,
    bool       outside)
{
  LinearLoc d=NB::target(s,l,rd.nrCells(),rd.nrCols() );
  if (d>=rd.nrCells())
    return outside;
  CellLoc d2= NB::target(rd.convert(s),l);
  return d2 == rd.convert(d);
}

}

void geo::NeighbourTest::testLinearDownStream()
{
  RasterDim rd(5,3);

  // all NBS are inside grid
  LinearLoc f=rd.convert(CellLoc(1,1));

  BOOST_CHECK(neighbourTarget(f,rd,1, false));
  BOOST_CHECK(neighbourTarget(f,rd,2, false));
  BOOST_CHECK(neighbourTarget(f,rd,3, false));
  BOOST_CHECK(neighbourTarget(f,rd,4, false));
  BOOST_CHECK(neighbourTarget(f,rd,5, false));
  BOOST_CHECK(neighbourTarget(f,rd,6, false));
  BOOST_CHECK(neighbourTarget(f,rd,7, false));
  BOOST_CHECK(neighbourTarget(f,rd,8, false));
  BOOST_CHECK(neighbourTarget(f,rd,9, false));

  BOOST_CHECK(nbTarget(f,rd,0, false));
  BOOST_CHECK(nbTarget(f,rd,1, false));
  BOOST_CHECK(nbTarget(f,rd,2, false));
  BOOST_CHECK(nbTarget(f,rd,3, false));
  BOOST_CHECK(nbTarget(f,rd,4, false));
  BOOST_CHECK(nbTarget(f,rd,5, false));
  BOOST_CHECK(nbTarget(f,rd,6, false));
  BOOST_CHECK(nbTarget(f,rd,7, false));

  // left NBs are outside
  f=rd.convert(CellLoc(1,0));

  BOOST_CHECK(neighbourTarget(f,rd,1, true ));
  BOOST_CHECK(neighbourTarget(f,rd,2, false));
  BOOST_CHECK(neighbourTarget(f,rd,3, false));
  BOOST_CHECK(neighbourTarget(f,rd,4, true ));
  BOOST_CHECK(neighbourTarget(f,rd,5, false));
  BOOST_CHECK(neighbourTarget(f,rd,6, false));
  BOOST_CHECK(neighbourTarget(f,rd,7, true ));
  BOOST_CHECK(neighbourTarget(f,rd,8, false));
  BOOST_CHECK(neighbourTarget(f,rd,9, false));

  // top/left NBs are outside
  f=rd.convert(CellLoc(1,0));

  BOOST_CHECK(neighbourTarget(f,rd,1, true ));
  BOOST_CHECK(neighbourTarget(f,rd,2, false));
  BOOST_CHECK(neighbourTarget(f,rd,3, false));
  BOOST_CHECK(neighbourTarget(f,rd,4, true ));
  BOOST_CHECK(neighbourTarget(f,rd,5, false));
  BOOST_CHECK(neighbourTarget(f,rd,6, false));
  BOOST_CHECK(neighbourTarget(f,rd,7, true ));
  BOOST_CHECK(neighbourTarget(f,rd,8, true ));
  BOOST_CHECK(neighbourTarget(f,rd,9, true ));

  // bottom/right NBs are outside
  f=rd.convert(CellLoc(rd.nrRows()-1,rd.nrCols()-1));

  BOOST_CHECK(neighbourTarget(f,rd,1, true ));
  BOOST_CHECK(neighbourTarget(f,rd,2, true ));
  BOOST_CHECK(neighbourTarget(f,rd,3, true ));
  BOOST_CHECK(neighbourTarget(f,rd,4, false));
  BOOST_CHECK(neighbourTarget(f,rd,5, false));
  BOOST_CHECK(neighbourTarget(f,rd,6, true ));
  BOOST_CHECK(neighbourTarget(f,rd,7, false));
  BOOST_CHECK(neighbourTarget(f,rd,8, false));
  BOOST_CHECK(neighbourTarget(f,rd,9, true ));
}

void geo::NeighbourTest::testConversion()
{
  BOOST_CHECK(LDD::toNB(1)==0);
  BOOST_CHECK(LDD::toNB(9)==7);
  BOOST_CHECK(NB::toLDD(0)==1);
  BOOST_CHECK(NB::toLDD(7)==9);
  // test Koenig lookup trick
  {
   NB::Code c=7;
   BOOST_CHECK(NB::toLDD(c)==9);
   BOOST_CHECK(NB::reverse(c)==0);
  }
}

void geo::NeighbourTest::testNBCode()
{
/* A one byte Neighbour bit code can be used to encode multiple neighbours.
 * Bit nrs set refer to the following ldd:
 *  5   6   7
 *   \  |  /
 *  3 - * - 4
 *    / |  \
 *  0   1   2
 *  AKA as the NBCode
 */
  CellLoc  from(100,50);
  BOOST_CHECK(NB::code(from,CellLoc(101,49))==0);
  BOOST_CHECK(NB::code(from,CellLoc(101,50))==1);
  BOOST_CHECK(NB::code(from,CellLoc(101,51))==2);
  BOOST_CHECK(NB::code(from,CellLoc(100,49))==3);
  BOOST_CHECK(NB::code(from,CellLoc(100,51))==4);
  BOOST_CHECK(NB::code(from,CellLoc( 99,49))==5);
  BOOST_CHECK(NB::code(from,CellLoc( 99,50))==6);
  BOOST_CHECK(NB::code(from,CellLoc( 99,51))==7);

  BOOST_CHECK( NB::diagonal(from,CellLoc(101,49)));
  BOOST_CHECK(!NB::diagonal(from,CellLoc(101,50)));
  BOOST_CHECK( NB::diagonal(from,CellLoc(101,51)));
  BOOST_CHECK(!NB::diagonal(from,CellLoc(100,49)));
  BOOST_CHECK(!NB::diagonal(from,CellLoc(100,51)));
  BOOST_CHECK( NB::diagonal(from,CellLoc( 99,49)));
  BOOST_CHECK(!NB::diagonal(from,CellLoc( 99,50)));
  BOOST_CHECK( NB::diagonal(from,CellLoc( 99,51)));
}

void geo::NeighbourTest::testDownStreamVisitorCell()
{
  /*! DownStreamVisitorCell uses bitfields for storage efficiency
   *  what is tested below is that the two bitfield members together
   *  fit in 4 bytes.
   */
   if (sizeof(size_t) == 4) {
    BOOST_CHECK(sizeof(geo::DownStreamVisitorCell) <= (sizeof(geo::CellLoc)+4));
   } else {
    // Bugzilla #133
    bool todoBitPackingOnNon32Bit=
      sizeof(geo::DownStreamVisitorCell) <= (sizeof(geo::CellLoc)+4);
    BOOST_WARN(todoBitPackingOnNon32Bit);
   }
}
