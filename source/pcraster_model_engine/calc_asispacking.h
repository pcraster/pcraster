#ifndef INCLUDED_CALC_ASISPACKING
#define INCLUDED_CALC_ASISPACKING

#include "stddefx.h"
#include "calc_spatialpacking.h"



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

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AsIsPacking               (const geo::RasterDim& rs);

                   AsIsPacking               (const AsIsPacking&) = default;

  /* virtual */    ~AsIsPacking              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  AsIsPacking&           operator=           (const AsIsPacking&) = delete;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  Field*         pack                (const Field* f)const override;
  const Field*   unpack              (const Field* f)const override;
  Field*         createSpatial       (VS vs)const override;

  size_t         toRasterId          (size_t fieldId)  const override;
  size_t         toFieldId           (size_t rasterId) const override;
  size_t         nrFieldCells        () const override;

  AsIsPacking*   createClone         () const override;
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
