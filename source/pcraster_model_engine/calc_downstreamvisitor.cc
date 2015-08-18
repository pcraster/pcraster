#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
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

calc::DownstreamVisitor::DownstreamVisitor(const LddGraph& graph):
  d_graph(graph)
{
}

calc::DownstreamVisitor::~DownstreamVisitor()
{
}

void calc::DownstreamVisitor::visitCatchment(
    LddGraph::Catchment const& c)
{
     startCatchment(c.d_pitId);
     LddGraph::DownConstIterator d = c.downBegin();
     LddGraph::DownConstIterator end = c.downEnd();
     for( ; d != end; ++d) {
       finishVertex(d->up());
       visitEdge(d->up(),d->down());
     }
     if (!d_graph.invalid(c.d_pitId))
         finishVertex(c.d_pitId);

     d = c.downBegin();
     for( ; d != end; ++d) {
       finishVertex2(d->up());
     }
     if (!d_graph.invalid(c.d_pitId))
         finishVertex2(c.d_pitId);
}

//! visit entire in downstream order
void calc::DownstreamVisitor::visitEntireLdd()
{
  for(LddGraph::Catchments::const_iterator i=d_graph.catchmentsBegin();
         i != d_graph.catchmentsEnd(); ++i) {
    visitCatchment(*i);
  }
}


void calc::DownstreamVisitor::visitEdge(size_t /* up */,size_t /* down */)
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

const calc::LddGraph& calc::DownstreamVisitor::graph() const
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
