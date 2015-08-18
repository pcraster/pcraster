#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
#endif

// Library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the DownStreamVisitor class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
   \param catchmentOutletPit location of catchment outlet, must have pit
                             value on \a lddMap
 */
calc::DownStreamVisitor::DownStreamVisitor(
    const fieldapi::ReadOnlyUint1& lddMap,
    const geo::CellLoc& catchmentOutletPit):
    d_lddMap(lddMap)
{
#ifdef DEBUG
   UINT1 pitV;
   PRECOND(d_lddMap.get(pitV,catchmentOutletPit));
   PRECOND(pitV==5);
#endif
   d_inProcess.push(geo::DownStreamVisitorCell(catchmentOutletPit));
   next();
}

//! dtor
calc::DownStreamVisitor::~DownStreamVisitor()
{
  //! in case of error we need to clean
  while (!d_inProcess.empty()) {
    // delete d_inProcess.top();
    d_inProcess.pop();
  }
}


//! advance to next cell in down stream order
void calc::DownStreamVisitor::next()
{
  if (!valid())
    return;
  while(! front().allUpstreamNeighboursVisited()) {
    geo::DownStreamVisitorCell& f(front());
    int rowNB,colNB;
    geo::intDownstreamCell(rowNB,colNB, f, f.d_nextNeighbourToVisit);
    UINT1 l;
    bool isUpstreamNB= (d_lddMap.get(l,rowNB,colNB) &&
                        geo::LDD::flowsTo(l, f.d_nextNeighbourToVisit));
    // next time look for next
    f.next(isUpstreamNB);
    // add isUpstreamNB
    if (isUpstreamNB)
      d_inProcess.push(geo::DownStreamVisitorCell(geo::CellLoc(rowNB,colNB)));
  }
}

//! advance to next cell in down stream order
void calc::DownStreamVisitor::operator++()
{
  d_inProcess.pop();
  next();
}

//! return current cell
geo::UpstreamNeighbourVisitor calc::DownStreamVisitor::operator*() const
{
  const geo::DownStreamVisitorCell& f(d_inProcess.top());
  return geo::UpstreamNeighbourVisitor(f,f.d_upstreamNeighbourDirs);
}

//! check if some left to be processed
bool calc::DownStreamVisitor::valid() const
{
  return !d_inProcess.empty();
}

//! return first;
geo::DownStreamVisitorCell& calc::DownStreamVisitor::front()
{
    return d_inProcess.top();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
