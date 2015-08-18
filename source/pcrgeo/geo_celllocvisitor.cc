#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! indices to point to cells
static struct { int deltaCol; int deltaRow; } lddDirection[10] = {
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

//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*! only done non-in-line to make bcc32 link work.
    If no single non-inline then bcc32 creates
    a depency on the test module of cellLocVistor, 
    which depends on the cppunit library
*/
geo::CellLocVisitor::CellLocVisitor(size_t nrRows, size_t nrCols):
   d_rd(nrRows,nrCols),
   d_currentCell(0,0)
{
}

geo::CellLocVisitor::CellLocVisitor(const RasterDim& rd):
   d_rd(rd),
   d_currentCell(0,0)
{
}

//! unchecked version
geo::CellLoc geo::CellLocVisitor::downstream(unsigned int lddVal) const
{
  DEVELOP_PRECOND(lddVal >=1 && lddVal <= 9);
  return CellLoc(
             row()+lddDirection[lddVal].deltaRow,
             col()+lddDirection[lddVal].deltaCol);
}

//! checked version
bool geo::CellLocVisitor::downstream(
  CellLoc& cl,
  unsigned int lddVal) const
{
  cl = downstream(lddVal);
  return d_rd.contains(cl.row(),cl.col());
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------
