#ifndef INCLUDED_CALC_ORDEROPERATIONS
#define INCLUDED_CALC_ORDEROPERATIONS

#include "stddefx.h"
#include "pcrtypes.h"
#include "calc_vspatial.h"



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
