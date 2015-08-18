#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_ACCUFLUX
#include "mldd_accuflux.h"
#define INCLUDED_MLDD_ACCUFLUX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_MLDD_WEIGHTMAP
#include "mldd_weightmap.h"
#define INCLUDED_MLDD_WEIGHTMAP
#endif



/*!
  \file
  This file contains the implementation of the Accuflux class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class AccufluxPrivate
{
public:

  AccufluxPrivate()
  {
  }

  ~AccufluxPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ACCUFLUX MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ACCUFLUX MEMBERS
//------------------------------------------------------------------------------

mldd::Accuflux::Accuflux(const mldd::WeightMap& w,
                         const REAL4* oldState,
                               REAL4* newState
                         ):
   DownstreamVisitor(w.rasterDim()),
   d_w(w),
   d_oldState(oldState),
   d_newState(newState)
{
  pcr::setMV(d_newState,w.rasterDim().nrCells());
}

mldd::Accuflux::~Accuflux()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
mldd::Accuflux& mldd::Accuflux::operator=(const Accuflux& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
mldd::Accuflux::Accuflux(const Accuflux& rhs)
{
}
*/

void mldd::Accuflux::initVertex(
   const Vertex& vC)
{
  size_t v=linear(vC);
  d_newState[v]=d_oldState[v];
}

void mldd::Accuflux::downstreamEdge(const Edge& e)
{
  size_t s,t;
  linear(s,t,e);
  DEVELOP_PRECOND(!pcr::isMV(d_newState[t]));
  double w=d_w[e];

  if (w != WeightMap::mvMark() && !pcr::isMV(d_newState[s]))
    d_newState[t]+=(w*d_newState[s]);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



