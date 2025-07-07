#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#define INCLUDED_CALC_DOWNSTREAMVISITOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_LDDGRAPH
#include "calc_lddgraph.h"
#define INCLUDED_CALC_LDDGRAPH
#endif
// Module headers.



namespace calc {
  // DownstreamVisitor declarations.
}



namespace calc {



//! visitor of a directed graph such as Ldd and Mldd
/*!
 * modelled after the Boost graph dfs en bfs visitors
 */
class DownstreamVisitor
{

  const LddGraph& d_graph;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DownstreamVisitor               (const LddGraph& graph);

                   DownstreamVisitor               (const DownstreamVisitor& other) = delete;

                   DownstreamVisitor& operator=    (const DownstreamVisitor& other) = delete;

     virtual       ~DownstreamVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             visitEntireLdd                   ();
  void             visitCatchmentOfPit              (LddGraph::Catchments::const_iterator c);
  void visitCatchment                               (LddGraph::Catchment const& i);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const LddGraph&  graph                            () const;
protected:
  virtual void     visitEdge                        (size_t /* up */,size_t /* down */);
  virtual void     finishVertex                     (size_t /* v */);
  virtual void     finishVertex2                    (size_t /* v */);
  virtual void     startCatchment                   (size_t pitId);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
