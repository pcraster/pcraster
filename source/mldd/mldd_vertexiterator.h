#ifndef INCLUDED_MLDD_VERTEXITERATOR
#define INCLUDED_MLDD_VERTEXITERATOR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_ITERATOR_ADAPTORS
#include <boost/iterator_adaptors.hpp>
#define INCLUDED_BOOST_ITERATOR_ADAPTORS
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_MLDD_EDGEVERTEX
#include "mldd_edgevertex.h"
#define INCLUDED_MLDD_EDGEVERTEX
#endif
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif

namespace mldd {
  // VertexIterator declarations.
}



namespace mldd {


//! implements graph_traits<mldd::DagRaster>::vertex_iterator
  class  VertexIterator:
   public boost::iterator_facade<
     VertexIterator,
     Vertex const,
     //boost::multi_pass_input_iterator_tag
     boost::forward_traversal_tag
   >
{

  friend class VertexIteratorTest;

private:


   const  DagRaster* d_g;
   Vertex            d_v;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  //! not default constructible
                   VertexIterator               ();

  VertexIterator&           operator=           (VertexIterator const& rhs);

                   VertexIterator               (VertexIterator const& rhs);


                   VertexIterator               (const DagRaster& g,
                                                 Vertex v);

                   VertexIterator               (Vertex v);

  /* virtual */    ~VertexIterator              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  inline void increment();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

    void initialize(Vertex&) const { }
/*
   Vertex const&  dereference() const
    { return d_v; }
*/
   Vertex const& dereference() const
    { return d_v; }

    bool equal(const VertexIterator& lhs) const
    { DEVELOP_PRECOND(d_g == lhs.d_g);
      return d_v == lhs.d_v;
    }

};

/*
 * typedef boost::iterator_adaptor<
 *       VertexIterator,
 *       // mldd_vertex_iterator_policies,
 *       Vertex, mldd::Vertex, const mldd::Vertex*,
 *       boost::forward_traversal_tag,
 *       std::ptrdiff_t
 * > vertex_iterator;
*/

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

void VertexIterator::increment() {
 d_v = d_g->nextVertex(d_v);
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace mldd

#endif
