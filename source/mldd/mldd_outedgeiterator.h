#ifndef INCLUDED_MLDD_OUTEDGEITERATOR
#define INCLUDED_MLDD_OUTEDGEITERATOR



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
#ifndef INCLUDED_MISC
#include "misc.h"  // FIRSTBITSET_TYPE
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif

// Module headers.
#ifndef INCLUDED_MLDD_EDGEVERTEX
#include "mldd_edgevertex.h"
#define INCLUDED_MLDD_EDGEVERTEX
#endif



namespace mldd {
  // OutEdgeIterator declarations.
}

namespace mldd {

//! implements graph_traits<mldd::DagRaster>::out_edge_iterator
class OutEdgeIterator :
   public boost::iterator_facade<
     OutEdgeIterator,
     Edge,
     std::forward_iterator_tag
   >
{

private:
  Vertex        d_v;
  /*!
   * keep track of the edges not yet processed
   */
  unsigned char d_outflowMask;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Assignment operator. NOT IMPLEMENTED.
  OutEdgeIterator&           operator=           (const OutEdgeIterator& rhs)
  {
    d_outflowMask = rhs.d_outflowMask;
    d_v           = rhs.d_v;
    return *this;
  }

  //! Copy constructor. NOT IMPLEMENTED.
                   OutEdgeIterator               (const OutEdgeIterator& rhs):
    d_v(rhs.d_v),
    d_outflowMask(rhs.d_outflowMask)
                   {}

                   OutEdgeIterator               () {};

  /* virtual */    ~OutEdgeIterator              () {};

     OutEdgeIterator(
          const       Vertex& v,
          int         outflowMask,
          geo::NB::Code start):
          d_v(v),
          d_outflowMask(outflowMask)
      {
        DEVELOP_PRECOND(start < 8);
      }
     // the end iterator
     OutEdgeIterator(
          const       Vertex& v):
          d_v(v),
          d_outflowMask(0)
      {
      }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void increment() {
    if (d_outflowMask) {
      // unset current
      size_t pos=FIRSTBITSET_TYPE(d_outflowMask,char);
      d_outflowMask^=(1<<pos);
    }
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool operator!=(const OutEdgeIterator& e) const {
        return e.d_outflowMask != d_outflowMask || e.d_v != d_v;
  }
  bool operator==(const OutEdgeIterator& e) const {
        return e.d_outflowMask == d_outflowMask && e.d_v == d_v;
  }


  Edge edge() const {
        DEVELOP_PRECOND(d_outflowMask);
        size_t pos=FIRSTBITSET_TYPE(d_outflowMask,char);
        return Edge(d_v,geo::NB::target(d_v,pos));
  }
  Edge operator*() const {
    return edge();
  }
  //! alternative for OutEdgeIterator::begin() == OutEdgeIterator::end()
  /*! does the iterator still has something to deliver?
   *  this alternative is faster since we do not have have to construct an
   *  end() iterator to compare against
   */
  bool any() const {
        return d_outflowMask;
  }
};
/*
 * typedef OutEdgeIterator out_edge_iterator;
 * struct mldd_out_edge_iterator_policies
 * {
 *     template <typename Iter>
 *     static void initialize(Iter& ) { }
 *
 *     template <typename Iter>
 *     static void increment(Iter& i)
 *     {i.base().next(); }
 *
 *
 *  // template <typename Iter>
 *  // static void decrement(Iter& i)
 *  // { }
 *  //
 *
 *     template <typename Iter>
 *     static  Edge dereference(const Iter& i)
 *     { return i.base().edge(); }
 *
 *
 *     template <typename Iter>
 *     static bool equal(const Iter& x, const Iter& y)
 *     { return x.base() == y.base(); }
 * };
 *
 *typedef boost::iterator_adaptor<
 *      OutEdgeIterator,
 *      Edge, Edge
 *      //Vertex, mldd::Vertex, const mldd::Vertex*,
 *> out_edge_iterator;
 */

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace mldd

#endif
