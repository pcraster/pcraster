#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LDDGRAPH
#include "calc_lddgraph.h"
#define INCLUDED_CALC_LDDGRAPH
#endif

// Library headers.
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif
// Module headers.
#ifndef INCLUDED_CALC_IFIELDRDCONVERSION
#include "calc_ifieldrdconversion.h"
#define INCLUDED_CALC_IFIELDRDCONVERSION
#endif



/*!
  \file
  This file contains the implementation of the LddGraph class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class LddGraphPrivate
{
public:

  LddGraphPrivate()
  {
  }

  ~LddGraphPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LDDGRAPH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LDDGRAPH MEMBERS
//------------------------------------------------------------------------------

/*
void calc::LddGraph::initPitIds()
{

  d_fieldIdToPitId = std::vector<PitId>(nrVertices(),
                     std::numeric_limits<PitId>::max());

  for(PitId c=0; c < d_catchments.size(); ++c)
    d_fieldIdToPitId[d_catchments[c].d_pitId] = c;

  for(UpConstIterator i=upBegin();i!=upEnd();++i)
    d_fieldIdToPitId[i->up()] = d_fieldIdToPitId[i->down()];
}
*/

/*!
 * \todo
 *   downConstIterators most used, is there a perfomance penalty in
 *   using reverse_iterator's instead of iterator's? If so, uncomment
 *   reverse operations and swap iterator type for up and down.
 */
calc::LddGraph::LddGraph(
                       const UINT1 *lddField,
                       const IFieldRDConversion& conv):
  RasterGraph(conv.nrFieldCells())
/*
 * disabled
  // initial with a high value identifying not flowing to a pit
    d_fieldIdToPitId(conv.nrFieldCells(),std::numeric_limits<PitId>::max())
 */
{
  size_t fieldLen=nrVertices();

  d_invalidFieldId=fieldLen;

  d_edge.reserve(fieldLen);
  d_mv.reserve(fieldLen);

  struct Check {
    boost::dynamic_bitset<>  d_added;
    Check(size_t n):
      d_added(n)
    {
    }
    void add(size_t i) {
      PRECOND(i<d_added.size());
      /* in current setup, starting from
       * pit this can not happen for an Ldd,
       * it means that a cells point to
       * 2 other cells as downstream cell
       * by definition an ldd only encode a
       * single direction, but for mldd it is
       * possible
       */
      DEVELOP_PRECOND(!d_added[i]);
      if (d_added[i])
         throw Unsound();
      d_added.set(i);
    }
    void finish() const {
      if (d_added.count() != d_added.size())
         throw Unsound();
    }
  } check(fieldLen);

  std::vector<size_t> catchmentBegin;
  std::vector<size_t> catchmentEnd;

  for(size_t p=0; p<fieldLen; ++p) {
    switch(lddField[p]) {
     case MV_UINT1: d_mv.push_back(p);
                    check.add(p);
                    break;
     case LDD_PIT:
      catchmentBegin.push_back(d_edge.size());
      d_catchments.push_back(Catchment(p));

      /* evaluate from the pit upstream, in breadth first order
       * only breadth first is the correct ordering for both
       * downstream and upstream
       */
      // targets of downstream edge
      std::stack<geo::LinearLoc> targets;
      targets.push(p);
      while(!targets.empty()) {
        FieldId        targetF=targets.top();

        geo::LinearLoc targetR=conv.toRasterId(targetF);
        targets.pop();
        for(geo::NB::Code nb=0; nb<8; ++nb) {
         geo::LinearLoc sourceR=conv.rasterDim().target<geo::NB>(targetR,nb);
         if (sourceR!=conv.invalidId()) {
          // valid neighbour
          FieldId sourceF = conv.toFieldId(sourceR);
          if (sourceF != conv.invalidId() &&
              lddField[sourceF] != MV_UINT1 &&
              targetR==conv.rasterDim().target<geo::LDD>(sourceR,lddField[sourceF])) {
              // neighbour sourceF flows to targetF
              addEdge(sourceF,targetF);
              targets.push(sourceF);
          }
         }
        }
        check.add(targetF);
      } // eowhile add cachment
    }
  }
  check.finish();

  d_edge.resize(d_edge.size());
  d_mv.resize(d_mv.size());

  catchmentBegin.push_back(d_edge.size());
  for(size_t i=0; i < d_catchments.size(); ++i) {
    d_catchments[i].d_beginEdge = d_edge.begin()+catchmentBegin[i];
    d_catchments[i].d_endEdge   = d_edge.begin()+catchmentBegin[i+1];
  }

  // initPitIds();
}

/*! \brief create from an existing LddGraph \a org with MV's deleted
 *
 *  Used in multi-pass algorithms (like DynamicWave and LddRouting) since
 *  to prevent checking each time.
 *
 *  Each vertex V having a true value in \a isMV will remove:
 *  - all edges on the path from V up to but not including the pit
 *  - all edges have vertex V as target/down vertex
 *
 *  Pits are only removed if set in \a isMV, not by propagation.
 *  As a side effect the remaining graph does not garantuee each path to end in
 *  a pit.
 *
 *  \pre propagateDownstream must be true FTTB, upstream not yet implemented
 *
 *  No optimzation is considered, this code should only be executed in the
 *  case where users go wrong and introduce MV's in arguments of an 
 *  ldd traversal algorithm
 *
 *  \todo NOG BETER: RasterGraph kan de d_mv zetten o.b.v. isMV, 
 *  wat daar niet in zit, zit wel in de LddGraph, wordt generieker
 *  ook voor andere graphs ??
 */
calc::LddGraph::LddGraph(
       const LddGraph& org,
       const boost::dynamic_bitset<>& isMV,
       bool  propagateDownstream):
  RasterGraph(org.nrVertices()),
  d_invalidFieldId(org.d_invalidFieldId)
{
  POSTCOND(propagateDownstream);

  // each FieldId is represented in isMV
  POSTCOND(isMV.size()==org.nrVertices());

  // initiate
  boost::dynamic_bitset<> remove(isMV);

  // propagate
  if (propagateDownstream)
   for(DownConstIterator i=org.downBegin(); i!=org.downEnd(); ++i) {
    if (remove[i->up()])
     remove[i->down()] = 1;
   }
  else {
   // FTTB propagateUpstream not yet tested
   // I think up traversal of d_edge
   // checkin target marking source
   POSTCOND(FALSE);
  }

  d_mv=org.d_mv;

  d_edge.reserve(org.d_edge.size()); // avoid invalidation of iters
  for(Catchments::const_iterator orgC=org.d_catchments.begin();
      orgC != org.d_catchments.end(); ++orgC) {
    Catchment newC=*orgC;
    if (orgC->d_pitId != d_invalidFieldId) {
     if (isMV[orgC->d_pitId]) {
      d_mv.push_back(orgC->d_pitId);
      newC.d_pitId = d_invalidFieldId;
     } else {
       // pits are only removed if set
       // in isMV, not by propagation
      // undo propagation
      // otherwise ALL edges ending in pit will be removed to
      remove[orgC->d_pitId]=0;
     }
    }

    // Don't assign d_beginEdge just yet. The d_edge collection is changed in
    // the next loop, which potentially invalidates the iterators...
    // Although this won't happen in practice (see reserve call above), Visual
    // Studio's STL does assert this situation.
    size_t idOfBeginEdge = d_edge.size();
    // newC.d_beginEdge=d_edge.end();

    for(Edges::const_iterator e=orgC->d_beginEdge; e!=orgC->d_endEdge; ++e) {
      if (remove[e->up()])
        d_mv.push_back(e->up());
      else {
        // for a still valid edge
        // both e->up() and e->down() should be unmarked
        if (!remove[e->down()])
          d_edge.push_back(*e);
      }
    }

    assert(idOfBeginEdge <= d_edge.size());
    newC.d_beginEdge = d_edge.begin() + idOfBeginEdge;
    newC.d_endEdge = d_edge.end();

    if(newC.d_pitId != d_invalidFieldId || newC.d_beginEdge != newC.d_endEdge) {
      d_catchments.push_back(newC);
    }
  }

  // initPitIds();
}

void calc::LddGraph::addEdge(FieldId sV, FieldId tV) {
  Edge ve;
  ve.d_sourceVertex=sV;
  ve.d_targetVertex=tV;
  d_edge.push_back(ve);
}

/* NOT IMPLEMENTED
//! Copy constructor.
calc::LddGraph::LddGraph(LddGraph const& rhs)

  : Base(rhs)

{
}
*/



calc::LddGraph::~LddGraph()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::LddGraph& calc::LddGraph::operator=(LddGraph const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



