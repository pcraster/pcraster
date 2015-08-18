#ifndef INCLUDED_CALC_SPATIALPACKING
#define INCLUDED_CALC_SPATIALPACKING



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif

// Module headers.
#ifndef INCLUDED_CALC_IFIELDRDCONVERSION
#include "calc_ifieldrdconversion.h"
#define INCLUDED_CALC_IFIELDRDCONVERSION
#endif



namespace calc {
  // SpatialPacking declarations.
  class Field;
}



namespace calc {

/*! Manages how the datavalues within a Spatial object are packed within the
 *  runtime environment (RunTimeEnv)
 */
class SpatialPacking: public IFieldRDConversion
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  SpatialPacking&           operator=           (const SpatialPacking&);

  //  Copy constructor default for createClone
  //               SpatialPacking               (const SpatialPacking&);

  //! the raster space it acts on
  const geo::RasterDim  d_rd;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpatialPacking               (const geo::RasterDim& rd);

     virtual       ~SpatialPacking              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const geo::RasterDim& rasterDim           () const;

  virtual Field*         pack                (const Field* f)const=0;
  //! may return packed itself or a new created Field
  /*!
   * caller must delete iff result != \a packed
   */
  virtual const Field *unpack             (const Field* packed)const=0;

  virtual Field *createSpatial            (VS vs)const=0;

  virtual SpatialPacking*           createClone        ()const=0;
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
