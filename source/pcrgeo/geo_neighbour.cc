#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif

// Library headers.
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.




//------------------------------------------------------------------------------
// DEFINITION OF STATIC XXXXX MEMBERS
//------------------------------------------------------------------------------

namespace geo {

/* names as winddirection */
const char* lddValueName[10]={ "",
    "SW","S"  ,"SE",
    "W ","Pit","E",
    "NW","N"  ,"NE"};

/* A one byte Neighbour bit code can be used to encode multiple neighbours.
 * Bit nrs set refer to the following ldd:
 *  5   6   7
 *   \  |  /
 *  3 - * - 4
 *    / |  \
 *  0   1   2
 *  AKA as the NBCode
 */

struct Delta {
 int deltaCol,
 deltaRow;
};

static Delta lddNBCode[8] = {
               {-1,   1},  /* 0 */
               { 0,   1},  /* 1 */
               { 1,   1},  /* 2 */
               {-1,   0},  /* 3 */
               { 1,   0},  /* 4 */
               {-1,  -1},  /* 5 */
               { 0,  -1},  /* 6 */
               { 1,  -1}}; /* 7 */

//! indices to point to cells
static Delta lddDirection[10] = {
               { 0,   0},
               {-1,   1},    /* 1 */
               { 0,   1},    /* 2 */
               { 1,   1},    /* 3 */
               {-1,   0},    /* 4 */
               { 0,   0},    /* LDD_PIT */
               { 1,   0},    /* 6 */
               {-1,  -1},    /* 7 */
               { 0,  -1},    /* 8 */
               { 1,  -1} };  /* 9 = NR_LDD_DIR */
  /* local drain direction maps have values for directions as
   * following:
   *  7   8   9
   *   \  |  /
   *  4 - 5 - 6
   *    / |  \
   *  1   2   3
   */


} // namespace geo

//! return cell location of down stream cell
/*!
 *  \pre result location is valid
 */
geo::CellLoc geo::LDD::target(
      const geo::CellLoc& from,
      LDD::Code           dir)
{
#ifdef DEBUG_DEVELOP
  PRECOND(LDD::valid(dir));
  PRECOND(static_cast<int>(from.row())+lddDirection[dir].deltaRow >= 0);
  PRECOND(static_cast<int>(from.col())+lddDirection[dir].deltaCol >= 0);
#endif
  return CellLoc(
     from.row()+lddDirection[dir].deltaRow,
     from.col()+lddDirection[dir].deltaCol);
}

//! return cell location of neigbour
geo::CellLoc geo::NB::target(
      const geo::CellLoc& from,
      NB::Code dir)
{
  return CellLoc(
     from.row()+lddNBCode[dir].deltaRow,
     from.col()+lddNBCode[dir].deltaCol);
}

//! ctor
/*!
   create with a location and bit array coding all nb's as ldd dirs
 */
geo::UpstreamNeighbourVisitor::UpstreamNeighbourVisitor(
      const geo::CellLoc& l, unsigned short int upstreamNeighbourDirs):
       geo::CellLoc(l),
       d_upstreamNeighbourDirs(upstreamNeighbourDirs),
       d_nextNeighbourToVisit(0)
{
}

//! advance
void geo::UpstreamNeighbourVisitor::operator++()
{
  while (valid()) {
    if (d_upstreamNeighbourDirs  & (1<<d_nextNeighbourToVisit))
      break;
    d_nextNeighbourToVisit++;
  }
}

//! current neighbour, set by ++
geo::CellLoc geo::UpstreamNeighbourVisitor::nb() const
{
  return LDD::downstreamCell(*this, (1<<d_nextNeighbourToVisit));
}

//------------------------------------------------------------------------------
// DEFINITION OF XXXXX MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/* how to translate a ldd value to which bit nr of the receiving
 * cell; the recieving cells encodes its incoming neigbours
 */
/*
static size_t lddVal2InflowNBCodes[10] = { 0,
               7,6,5,
               4,0,3,
               2,1,0 };
size_t geo::lddVal2InflowNBCode(size_t lddVal) {
  DEVELOP_PRECOND(lddVal < 10);
  return lddVal2InflowNBCodes[lddVal];
}

//! ldd (outflow) value to outflow bit code
size_t geo::lddVal2OutflowNBCode(size_t lddVal)
{
  PRECOND(lddVal!=5);
  return lddVal-1-(lddVal>5);
}
*/

//! compute cell loc (rowNB,colNB)
void geo::intDownstreamCell(
      int& rowNB,
      int& colNB,
      const geo::CellLoc& from,
      unsigned int fromLddVal)
{
#ifdef DEBUG_DEVELOP
  PRECOND(fromLddVal <= 9);
#endif
  rowNB = from.row()+lddDirection[fromLddVal].deltaRow;
  colNB = from.col()+lddDirection[fromLddVal].deltaCol;
}


namespace geo {
/*!
 * \return
 *    d, the result, neigbour downstream cell or
 *       std::limits<LinearLoc>::max()
 *       if neigbour is outside of map
 *
 * \todo
 *   measure if outOfRange should be done full or short circuit
 */
static inline LinearLoc adjLinearLoc(
      LinearLoc  s, /* source */
      size_t     nrCells,
      size_t     nrCols,
      Delta      l)
{
  /*
   * colInc,  d=s+1;       outOfRange=(s%nrCols)==0 ;
   *                                 =((d-1)%nrCols)==0 ;
   * colDec,  d=s-1;       outOfRange=(d%nrCols)==0 ;
   * rowInc,  d=s+nrCols;  outOfRange= d>=nrCells;
   * rowDec,  d=s-nrCols;  outOfRange= d<0
   *                                ; unsigned overflow ->
   *                                 = d >= nrCells
   */
  LinearLoc result[2] = { s, std::numeric_limits<LinearLoc>::max() };

#define colInc  (l.deltaCol == 1)
#define colDec  (l.deltaCol == -1)

  // inc/dec result[0] to become d

  // row inc/dec
  result[0]+=(l.deltaRow*nrCols);
  // row dec/inc
  result[0]+=l.deltaCol;
  bool outOfRange =
   // row decrement yield this
   // row increment yield this after
   //  overflow on the (rather) save assumption:
   //  --> std::numeric_limits<LinearLoc>::max  >= (nrCells+nrCols)
   (result[0] >= nrCells) ||
   // on col decrement a source
   // on the left border will get out of range
   (colDec && !(s % nrCols)) ||
   // on col increment a result
   // on the left border, indicates a row wrap
   (colInc && !(result[0] % nrCols));
  return result[outOfRange];
}

}

/*!
 * \return
 *    d, the result, neigbour downstream cell or
 *       std::limits<LinearLoc>::max()
 *       if neigbour is outside of map
 */
geo::LinearLoc geo::LDD::target(
      LinearLoc  s, /* source */
      NB::Code    l,
      size_t     nrCells,
      size_t     nrCols)
{
  return adjLinearLoc(s,nrCells,nrCols,lddDirection[l]);
}

/*!
 * \return
 *    d, the result, neigbour downstream cell or
 *       std::limits<LinearLoc>::max()
 *       if neigbour is outside of map
 */
geo::LinearLoc geo::NB::target(
      LinearLoc  s, /* source */
      NB::Code    l,
      size_t     nrCells,
      size_t     nrCols)
{
  return adjLinearLoc(s,nrCells,nrCols,lddNBCode[l]);
}

//! compute NB::code from 2 neighbours \a from and \a to
/*!
 * \pre
 *   to != from, to and from are neighbours
 */
geo::NB::Code geo::NB::code(const CellLoc& from, const CellLoc& to)
{
  int deltaRow=to.row()-from.row();
  int deltaCol=to.col()-from.col();
  DEVELOP_PRECOND(to != from);
  DEVELOP_PRECOND(deltaRow >= -1 && deltaRow <= 1);
  DEVELOP_PRECOND(deltaCol >= -1 && deltaCol <= 1);
  Code  delta[/*row*/3][/*col*/3] = {
       { 5, 6, 7 },
       { 3,99, 4 },
       { 0, 1, 2 } };
  DEVELOP_PRECOND(delta[deltaRow+1][deltaCol+1] < 8);
  return delta[deltaRow+1][deltaCol+1];
}

//! check if 2 neighbours \a from and \a to are diagonal neighours
/*!
 * \pre
 *   to != from, to and from are neighbours
 */
bool geo::NB::diagonal(const CellLoc& from, const CellLoc& to)
{
  int deltaRow=to.row()-from.row();
  int deltaCol=to.col()-from.col();
  DEVELOP_PRECOND(to != from);
  DEVELOP_PRECOND(deltaRow >= -1 && deltaRow <= 1);
  DEVELOP_PRECOND(deltaCol >= -1 && deltaCol <= 1);
  return deltaRow != 0 && deltaCol != 0;
}

#ifdef UITWERKEN
//! closeNB != diagonalNeigbour
/*!
 *  nb is one of the eight neighbours of
 *   l, is a diagonal or a close neighbour
 */
bool geo::closeNB(
      LinearLoc  l,
      LinearLoc  nb,
      size_t     nrCols)
{
  return
   // both on same column
   (l%nrCols)==(nb%nrCols)
     ||
   // both on same row
   std::abs(l-nb)==1; /* (l^nb) == 1 ??? */
}
leftBorderExclMaskNbCode= (1<<0)|(1<<3)|(1<<5);
topBorderExclMaskNbCode = (1<<5)|(1<<6)|(1<<7);
id (row==0) nbs^=topBorderExclMaskNbCode;
 -->
 nbs^=
  (((row==0)-1)&topBorderExclMaskNbCode)
  (row==0)-1 should overflow to full-1 mask or all-0 mask
#endif
