#ifndef INCLUDED_CALC_LDDDOWNSTREAM
#define INCLUDED_CALC_LDDDOWNSTREAM



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_DYNAMIC_BITSET
#include <boost/dynamic_bitset.hpp>
#define INCLUDED_BOOST_DYNAMIC_BITSET
#endif

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
// Module headers.
#ifndef INCLUDED_CALC_RASTERGRAPH
#include "calc_rastergraph.h"
#define INCLUDED_CALC_RASTERGRAPH
#endif



namespace calc {
  // LddDownstream declarations.
}



namespace calc {



//! as ldd raster
/*!
    as ldd type, encode to what vertex (cell) the FieldId flows
 */
class LddDownstream : public RasterGraph
{
public:
  typedef struct Unsound: public com::Exception {
    Unsound():
     com::Exception("Unsound ldd")
     {}
  };

  //! a single edge from d_sourceVertex to d_targetVertex
  typedef struct VertexEdges {
    FieldId d_sourceVertex;
    FieldId d_targetVertex;
    FieldId source() const { return d_sourceVertex; }
    FieldId target() const { return d_targetVertex; }
    //! upstream
    FieldId up()     const { return d_sourceVertex; }
    //! downstream
    FieldId down()   const { return d_targetVertex; }
  };
  typedef std::vector<VertexEdges>::iterator               upIterator;
  typedef std::vector<VertexEdges>::const_iterator         upConstIterator;

  typedef std::vector<VertexEdges>::reverse_iterator       downIterator;
  typedef std::vector<VertexEdges>::const_reverse_iterator downConstIterator;
private:
  /*! order as such as that iterating from begin() to end() will
      garantuee visiting in downstream order; the subcatchment of
      d_sourceVertex is already visited
   */
  std::vector<VertexEdges> d_edge;
  //! true if edge, is diagonal between cells, false if horizontal or vertical
  boost::dynamic_bitset<>  d_diagonal;
  std::vector<FieldId>     d_pit;

  void addEdge(FieldId sV, FieldId tV);


private:

  //! Assignment operator. NOT IMPLEMENTED.
  LddDownstream&           operator=           (LddDownstream const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LddDownstream               (LddDownstream const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LddDownstream               (
                       const UINT1 *lddField,
                       const IFieldRDConversion& conv);

  /* virtual */    ~LddDownstream              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  upIterator upBegin() {
    return d_edge.begin();
  }
  upIterator upEnd() {
    return d_edge.end();
  }
  downIterator downBegin() {
    return d_edge.rbegin();
  }
  downIterator downEnd() {
    return d_edge.rend();
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
