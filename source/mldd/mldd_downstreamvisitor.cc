#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_DOWNSTREAMVISITOR
#include "mldd_downstreamvisitor.h"
#define INCLUDED_MLDD_DOWNSTREAMVISITOR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DownstreamVisitor class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class DownstreamVisitorPrivate
{
public:

  DownstreamVisitorPrivate()
  {
  }

  ~DownstreamVisitorPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------

mldd::DownstreamVisitor::DownstreamVisitor(const geo::RasterDim rd):
  d_rd(rd)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
mldd::DownstreamVisitor::DownstreamVisitor(DownstreamVisitor const& rhs)

  : Base(rhs)

{
}
*/



mldd::DownstreamVisitor::~DownstreamVisitor()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
mldd::DownstreamVisitor& mldd::DownstreamVisitor::operator=(DownstreamVisitor const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void mldd::DownstreamVisitor::initVertex(const Vertex& )
{
}
void mldd::DownstreamVisitor::downstreamEdge(const Edge& )
{
}
void mldd::DownstreamVisitor::finishVertex(const Vertex& )
{
}

void mldd::DownstreamVisitor::linear(size_t& source, size_t& target, const Edge& e) const
{
  e.linear(source,target,d_rd);
}

size_t mldd::DownstreamVisitor::linear(const Vertex& v) const
{
  return d_rd.convert(v);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



