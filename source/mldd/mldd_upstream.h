#ifndef INCLUDED_MLDD_UPSTREAM
#define INCLUDED_MLDD_UPSTREAM

#include "stddefx.h"
#include "geo_simpleraster.h"
#include "mldd_downstreamvisitor.h"



namespace mldd {
  // Upstream declarations.
  class WeightMap;
}



namespace mldd {



//! Function object that implements the pcrcalc upstream function
class Upstream : public DownstreamVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Upstream&           operator=           (const Upstream& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Upstream               (const Upstream& rhs);

  const mldd::WeightMap&                   d_w;
  const REAL4*                             d_in;
  REAL4*                                   d_result;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Upstream               (const mldd::WeightMap& w,
                                           const REAL4* in,
                                           REAL4* result);

  /* virtual */    ~Upstream              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void initVertex    (const Vertex& v);
  void downstreamEdge(const Edge& e);

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



} // namespace mldd

#endif
