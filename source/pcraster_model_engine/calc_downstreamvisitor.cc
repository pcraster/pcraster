#include "stddefx.h"
#include "calc_downstreamvisitor.h"

/*!
  \file
  This file contains the implementation of the DownstreamVisitor class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

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

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF DOWNSTREAMVISITOR MEMBERS
//------------------------------------------------------------------------------

calc::DownstreamVisitor::DownstreamVisitor(const LddGraph &graph) : d_graph(graph)
{
}

calc::DownstreamVisitor::~DownstreamVisitor()
{
}

void calc::DownstreamVisitor::visitCatchment(LddGraph::Catchment const &c)
{
  startCatchment(c.d_pitId);
  auto d = c.downBegin();
  auto end = c.downEnd();
  for (; d != end; ++d) {
    finishVertex(d->up());
    visitEdge(d->up(), d->down());
  }
  if (!d_graph.invalid(c.d_pitId))
    finishVertex(c.d_pitId);

  d = c.downBegin();
  for (; d != end; ++d) {
    finishVertex2(d->up());
  }
  if (!d_graph.invalid(c.d_pitId))
    finishVertex2(c.d_pitId);
}

//! visit entire in downstream order
void calc::DownstreamVisitor::visitEntireLdd()
{
  for (auto i = d_graph.catchmentsBegin(); i != d_graph.catchmentsEnd(); ++i) {
    visitCatchment(*i);
  }
}

void calc::DownstreamVisitor::visitEdge(size_t /* up */, size_t /* down */)
{
}

void calc::DownstreamVisitor::finishVertex(size_t /* v */)
{
}

void calc::DownstreamVisitor::finishVertex2(size_t /* v */)
{
}

void calc::DownstreamVisitor::startCatchment(size_t /* pitId */)
{
}

const calc::LddGraph &calc::DownstreamVisitor::graph() const
{
  return d_graph;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TEMPLATE INSTANCES
//------------------------------------------------------------------------------
