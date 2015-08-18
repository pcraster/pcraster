#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LDDDOWNSTREAM
#include "calc_ldddownstream.h"
#define INCLUDED_CALC_LDDDOWNSTREAM
#endif

// Library headers.
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
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
  This file contains the implementation of the LddDownstream class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class LddDownstreamPrivate
{
public:

  LddDownstreamPrivate()
  {
  }

  ~LddDownstreamPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LDDDOWNSTREAM MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LDDDOWNSTREAM MEMBERS
//------------------------------------------------------------------------------

/*!
 * \todo
 *   downIterators most used, is there a perfomance penalty in
 *   using reverse_iterator's instead of iterator's? If so, uncomment
 *   reverse operations and swap iterator type for up and down.
 */
calc::LddDownstream::LddDownstream(
                       const UINT1 *lddField,
                       const IFieldRDConversion& conv):
  d_diagonal(conv.nrFieldCells())
{
  size_t fieldLen=conv.nrFieldCells();

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

  for(size_t p=0; p<fieldLen; ++p) {
    switch(lddField[p]) {
     case MV_UINT1: d_mv.push_back(p);
                    check.add(p);
                    break;
     case LDD_PIT:
      d_pit.push_back(p);
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
          if (lddField[sourceF] != MV_UINT1 &&
              targetR==conv.rasterDim().target<geo::LDD>(sourceR,lddField[sourceF])) {
              // neighbour sourceF flows to targetF
              d_diagonal[d_edge.size()]= lddField[sourceF] % 2;
              addEdge(sourceF,targetF);
              targets.push(sourceF);
           }
         }
        }
        check.add(targetF);
      }
    }
  }
  check.finish();

  d_edge.resize(d_edge.size());
  d_diagonal.resize(d_diagonal.size());
  d_mv.resize(d_mv.size());

/*
 * std::reverse(      d_edge.begin(),      d_edge.end());
 * std::reverse(d_diagonal.begin(),d_diagonal.end());
 */
}

void calc::LddDownstream::addEdge(FieldId sV, FieldId tV) {
  VertexEdges ve;
  ve.d_sourceVertex=sV;
  ve.d_targetVertex=tV;
  d_edge.push_back(ve);
}


/* NOT IMPLEMENTED
//! Copy constructor.
calc::LddDownstream::LddDownstream(LddDownstream const& rhs)

  : Base(rhs)

{
}
*/



calc::LddDownstream::~LddDownstream()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::LddDownstream& calc::LddDownstream::operator=(LddDownstream const& rhs)
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



