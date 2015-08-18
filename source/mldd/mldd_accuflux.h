#ifndef INCLUDED_MLDD_ACCUFLUX
#define INCLUDED_MLDD_ACCUFLUX



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_MLDD_DOWNSTREAMVISITOR
#include "mldd_downstreamvisitor.h"
#define INCLUDED_MLDD_DOWNSTREAMVISITOR
#endif



namespace mldd {
  // Accuflux declarations.
  class WeightMap;
}



namespace mldd {



//! Function object that implements the pcrcalc accuflux function
class Accuflux : public DownstreamVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Accuflux&           operator=           (const Accuflux& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Accuflux               (const Accuflux& rhs);

  const mldd::WeightMap&  d_w;
  const REAL4*            d_oldState;
  REAL4*                  d_newState;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Accuflux               ( const mldd::WeightMap& w,
                                            const REAL4* oldState,
                                            REAL4* newState);

  /* virtual */    ~Accuflux              ();

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
