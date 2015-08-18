#ifndef INCLUDED_MLDD_DAGRASTER
#define INCLUDED_MLDD_DAGRASTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif
// Module headers.

#ifndef INCLUDED_MLDD_OUTEDGEITERATOR
#include "mldd_outedgeiterator.h"
#define INCLUDED_MLDD_OUTEDGEITERATOR
#endif


namespace geo {
  // DagRaster declarations.
  class CellLoc;
}



namespace mldd {

  class GraphVisit;
  class VertexState;
  class DownstreamVisitor;

  //! throw if a cycle is detected
  struct NotADag {
  };


//! Neighbouring Raster cells connected as one ore more Directed Acyclic Graph (DAG)'s
/*!
 *  This DAG connect neighbour cells. The acyclicity is a condition that must
 *  be validated on the configuration.
 *  Note that a ldd is a special case of a DagRaster, it never has more than
 *  1 outflow direction.
 *
 *  NOTE flow to outside is not modelled, needs adaption of addFlowNB()
 *   and a single "sink" vertex to create such a graph
 */
class DagRaster
{
public:
  typedef geo::CellLoc vertex_descriptor;


  typedef unsigned char EightBits;
  EightBits d_0;
  typedef geo::SimpleRaster<EightBits> NBRaster;

private:
  friend class DagRasterTest;

  geo::RasterDim        d_rd;

  //! 2d matrix with the outflow bit codes
  /*! an outflow may point to outside the map
   */
  //! 2d matrix with the outflow bit codes
  /*! currently inflow from outside is not
   *  handled??
   */
  NBRaster d_outflowNB;
  //! 2d matrix with the inflow bit codes
  NBRaster d_inflowNB;

  typedef std::vector<Vertex> RTO;
  //! reverse topological order
  RTO d_rto;

private:

  // DEFAULT
  // DagRaster&           operator=           (const DagRaster& rhs);

  // Copy constructor. NOT IMPLEMENTED.
  //                DagRaster               (const DagRaster& rhs);

  void              addInflowNB             (const geo::CellLoc& c, geo::NB::Code nb);
  void              addOutflowNB            (const geo::CellLoc& c, geo::NB::Code nb);

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DagRaster               (const geo::RasterDim& rd);

  /* virtual */    ~DagRaster              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void                  addFlowNB          (const geo::CellLoc& outflowC, geo::LDD::Code lc);
  void                  addFlowNB          (size_t row,size_t col, geo::LDD::Code lc);
  void                  addFlow            (const geo::CellLoc& from,
                                            const geo::CellLoc& to);

  void                  remove             (const DagRaster& rm);

  void                  updateOrder();

  void                  clear();


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const geo::RasterDim& rasterDim() const;

  size_t                nrInflowNB(const geo::CellLoc& c) const;
  size_t                nrOutflowNB(const geo::CellLoc& c) const;
  bool                  hasInflowNB(const geo::CellLoc& c) const;
  bool                  hasOutflowNB(const geo::CellLoc& c) const;
  bool                  hasOutflowDir(geo::LinearLoc c,
                                      geo::NB::Code dir) const;
  bool                  isPit(const size_t c) const;

  void                  downstreamVisitor(DownstreamVisitor& dv) const;

  //! in CellLocVistor order
  Vertex                nextVertex(const Vertex& c) const;
  Vertex                beginVertex() const;
  Vertex                endVertex() const;

  OutEdgeIterator       beginOutEdge(const Vertex& c) const;
  OutEdgeIterator       endOutEdge(const   Vertex& c) const;

  size_t                nrVertices() const;
  size_t                nrEdges()    const;

  bool                  isVertex(geo::LinearLoc l) const;

  bool                  hasCycle() const;

  void                  conceptCheck() const;

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



} // namespace mldd

#endif
