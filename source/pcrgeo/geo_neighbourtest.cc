#define BOOST_TEST_MODULE pcraster geo neighbour
#include <boost/test/unit_test.hpp>
#include "geo_rasterdim.h"
#include "geo_neighbour.h"


static bool neighbourTarget(
    geo::LinearLoc  s,
    const geo::RasterDim&  rd,
    geo::LDD::Code  l,
    bool       outside)
{
  using namespace geo;

  LinearLoc d=LDD::target(s,l,rd.nrCells(),rd.nrCols());
  if (d>=rd.nrCells())
    return outside;
  CellLoc d2= LDD::target(rd.convert(s),l);
  return d2 == rd.convert(d);
}


static bool nbTarget(
    geo::LinearLoc  s,
    const geo::RasterDim&  rd,
    geo::NB::Code    l,
    bool       outside)
{
  using namespace geo;

  LinearLoc d=NB::target(s,l,rd.nrCells(),rd.nrCols() );
  if (d>=rd.nrCells())
    return outside;
  CellLoc d2= NB::target(rd.convert(s),l);
  return d2 == rd.convert(d);
}


BOOST_AUTO_TEST_CASE(linear_down_stream)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(conversion)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(nb_code)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(down_stream_visitor_cell)
{
  using namespace geo;

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
