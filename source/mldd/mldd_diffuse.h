#ifndef INCLUDED_MLDD_DIFFUSE
#define INCLUDED_MLDD_DIFFUSE



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
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif

// Module headers.
#ifndef INCLUDED_MLDD_EDGEVERTEX
#include "mldd_edgevertex.h"
#define INCLUDED_MLDD_EDGEVERTEX
#endif

namespace geo {
  class RasterSpace;
}

namespace mldd {
  // Diffuse declarations.
  class DagRaster;
}



namespace mldd {



//! implements the pcrcalc diffuse function
/*!
 * \todo
 *   FixedHead moet boolean kunnen zijn, doen na nieuwe API implementatie
 * \todo
 *   meeste input args, moeten ook nonspatial kunnen zijn, doen na nieuwe
 *   API implementatie
 * \todo
 *   MV tracking in DiffuseInit, DiffuseIter heeft er dan geen last meer van
 */
class Diffuse
{

  friend class DiffuseInit;
  friend class DiffuseIter;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Diffuse&           operator=           (const Diffuse& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Diffuse               (const Diffuse& rhs);

  //! 0:nondiagonal, 1:diagonal
  double                             d_cellSize[2];

  //! 1) Input/State
  const REAL4*              d_oldState;
  //! 2) Area
  const REAL4*              d_area;
  //! 3) Fixed Head
  const REAL4*              d_fixedHead;
  //! 4-12) DiffusionValue (8 - sized vector)
  std::vector<const REAL4*> d_diffusionValue;
  //! 13) iterations
  int                       d_nrIterations;

  /*! fixed edge flow term
   *
   *  index is visit sequence, value is computed from
   *  d_cellSize and d_diffusionValue
   *  drop = std::max(dem[src]-dem[dest],0)
   *  slope= drop/downstreamdist(edge(src,dest))
   *  edgeFlow =  (drop/downstreamdist(...)) * DiffusionValuePerIteration
   *  edgeFlow =  drop * (DiffusionValuePerIteration/downstreamdist(...))
   *  only drop is varying (on dem):
   *  fixedEdgeFlowTerm = DiffusionValuePerIteration/downstreamdist(...))
   */
  std::vector<REAL4>        d_fixedEdgeFlowTerm;

  //! created, MV where any (upstream) input is MV
  REAL4*                    d_totalOutflow;
  //! updated, set to MV where any (upstream) input is MV
  geo::ScalarSimpleRaster&  d_dem;

  size_t                    d_infinity, d_minInfinity;

  void initFlowTerm                   (const Edge& e);
  void initSet                        (size_t v);
  void setResultMV                    (size_t v);
  void addDem                         (size_t v);
  void checkDem                       (size_t v);
  REAL4 drop                          (const Edge& e) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
               Diffuse(geo::ScalarSimpleRaster&  dem,
                       REAL4*       totalOutflow,
                       const REAL4* oldState,
                       const REAL4* area,
                       const REAL4* fixedHead,
                       const std::vector<const REAL4 *>&
                                    diffusionValueInArgOrder,
                       INT4         nrIterations,
                       double       cellSize);

  /* virtual */    ~Diffuse              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void run(const DagRaster& rs);

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
