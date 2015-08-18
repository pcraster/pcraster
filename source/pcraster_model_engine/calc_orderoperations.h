#ifndef INCLUDED_CALC_ORDEROPERATIONS
#define INCLUDED_CALC_ORDEROPERATIONS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
// Module headers.
#ifndef INCLUDED_CALC_VSPATIAL
#include "calc_vspatial.h"
#define INCLUDED_CALC_VSPATIAL
#endif



namespace calc {
  // AreaOperations declarations.
}



namespace calc {

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

void orderOperation(
    REAL4 *res,
    IVSpatial<double> const& val,
    size_t len);



void areaOrderOperation(
    REAL4                    *res,
    IVSpatial<double> const&  expr,
    IVSpatial<INT4>   const&  areaClass,
    size_t len);



} // namespace calc

#endif
