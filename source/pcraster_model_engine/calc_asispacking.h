#ifndef INCLUDED_CALC_ASISPACKING
#define INCLUDED_CALC_ASISPACKING



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
#endif

// Module headers.



namespace calc {
  // AsIsPacking declarations.
}



namespace calc {



/*! The datavalues within a Spatial object are stored as is within the
 *  runtime environment (RunTimeEnv). A Spatial has exact the number of
 *  values as within the rasterDim
 */
class AsIsPacking: public SpatialPacking
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  AsIsPacking&           operator=           (const AsIsPacking&);

  //  Copy constructor default for createClone
  //               AsIsPacking               (const AsIsPacking&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AsIsPacking               (const geo::RasterDim& rs);

  /* virtual */    ~AsIsPacking              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Field*         pack                (const Field* f)const;
  const Field*   unpack              (const Field* f)const;
  Field*         createSpatial       (VS vs)const;

  size_t         toRasterId          (size_t fieldId)  const;
  size_t         toFieldId           (size_t rasterId) const;
  size_t         nrFieldCells        () const;

  AsIsPacking*   createClone         () const;
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
