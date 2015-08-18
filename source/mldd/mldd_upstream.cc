#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_UPSTREAM
#include "mldd_upstream.h"
#define INCLUDED_MLDD_UPSTREAM
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
  This file contains the implementation of the Upstream class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class UpstreamPrivate
{
public:

  UpstreamPrivate()
  {
  }

  ~UpstreamPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC UPSTREAM MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF UPSTREAM MEMBERS
//------------------------------------------------------------------------------

mldd::Upstream::Upstream(const WeightMap& w,
                         const REAL4* in,
                         REAL4* result):
   DownstreamVisitor(w.rasterDim()),
   d_w(w),
   d_in(in),
   d_result(result)
{
  pcr::setMV(d_result, d_w.rasterDim().nrCells());
}



mldd::Upstream::~Upstream()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
mldd::Upstream& mldd::Upstream::operator=(const Upstream& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
mldd::Upstream::Upstream(const Upstream& rhs)
{
}
*/

void mldd::Upstream::initVertex(
   const Vertex& v)
{
  d_result[linear(v)]=0;
}

void mldd::Upstream::downstreamEdge(const Edge& e)
{
  size_t s,t;
  linear(s,t,e);
  DEVELOP_PRECOND(!pcr::isMV(d_result[t]));
  double w=d_w[e];
  if (w != WeightMap::mvMark() && !pcr::isMV(d_in[s]))
    d_result[t]+=(w*d_in[s]);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



