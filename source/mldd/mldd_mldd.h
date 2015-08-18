#ifndef INCLUDED_MLDD_MLDD
#define INCLUDED_MLDD_MLDD



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
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif


// Module headers.



namespace calc {
  class RunTimeEnv;
}



namespace mldd {

class  DagRaster;


//! known as Multiple Ldd
class Mldd
{

  friend class MlddTest;

  //! the rasterspace
  geo::RasterSpace         d_rs;
  DagRaster               *d_dag;
  geo::ScalarSimpleRaster *d_dem;


private:

  //! Assignment operator. NOT IMPLEMENTED.
  Mldd&           operator=           (Mldd const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Mldd               (Mldd const& rhs);

  void             addOneLdd          (const UINT1 *ldd) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Mldd               (const geo::RasterSpace& rs);
                   Mldd               (calc::RunTimeEnv* rte);

  /* virtual */    ~Mldd              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             addStream          (const UINT1* ldd);
  void             setStream          (const std::vector<const UINT1*>& ldd);
  void             setDem             (const REAL4 *dem);

  void             diffuse            (REAL4*       totalOutflow,
                                       const REAL4* oldState,
                                       const REAL4* area,
                                       const REAL4* fixedHead,
                                       const std::vector<const REAL4 *>&
                                         diffusionValueInArgOrder,
                                       INT4         nrIterations);

  void             removeStream       (const std::vector<const UINT1 *>&
                                         markInArgOrder);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  geo::RasterSpace const& space       () const;

  void             getStream          (const std::vector<UINT1 *>& result) const;
  void             getWeight          (std::vector<REAL4 *>& result) const;
  void             getDem             (REAL4 *dem) const;
  void             upstream           (REAL4*       out,
                                       const REAL4* in) const;
  void             accuflux           (REAL4*       out,
                                       const REAL4* in) const;

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
