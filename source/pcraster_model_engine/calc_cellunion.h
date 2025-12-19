#ifndef INCLUDED_CALC_CELLUNION
#define INCLUDED_CALC_CELLUNION

#include "stddefx.h"
#include "csftypes.h"


namespace calc {

#ifdef GCC
#define RESTRICT __restrict__
#else
#define RESTRICT
#endif

struct CellPtr {
    //! ptr to spatial or nonspatial buffers
    union {
       void  * /* RESTRICT */  v; // generic access
       UINT1 *    RESTRICT     b; // byte, VS_B,VS_L
       INT4  *    RESTRICT     i; // int,  VS_N,VS_O
       REAL4 *    RESTRICT     f; // float, VS_S,VS_D
    };
};

} // namespace calc

#endif
