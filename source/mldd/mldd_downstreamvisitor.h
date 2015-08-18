#ifndef INCLUDED_MLDD_DOWNSTREAMVISITOR
#define INCLUDED_MLDD_DOWNSTREAMVISITOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif

// Module headers.
#ifndef INCLUDED_MLDD_EDGEVERTEX
#include "mldd_edgevertex.h"
#define INCLUDED_MLDD_EDGEVERTEX
#endif



namespace mldd {
  // DownstreamVisitor declarations.
}



namespace mldd {



//! Visitor with an init vertex and do downstream edge
class DownstreamVisitor
{
  //! only used for linear conversion
  /*!
   * \todo
   *  get rid of this one
   */
  const geo::RasterDim  d_rd;

private:

  //! Assignment operator. DEFAULT
  // DownstreamVisitor&           operator=           (const DownstreamVisitor& rhs);

  //! Copy constructor. DEFAULT
  //                 DownstreamVisitor               (const DownstreamVisitor& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DownstreamVisitor              (const geo::RasterDim rd);

   virtual        ~DownstreamVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  //! first action on vertex
  virtual void initVertex    (const Vertex& v);
  /*!
   * note that initVertex is already called on both e.source() and e.target()
   *  DownstreamVisitor garantuees that
   */
  virtual void downstreamEdge (const Edge& e);
  //! called if all in- and out- edges of vertex v are visited with downstreamEdge()
  virtual void finishVertex  (const Vertex& v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void   linear(size_t& source, size_t& target, const Edge& e) const;
  size_t linear(const Vertex& v) const;

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
