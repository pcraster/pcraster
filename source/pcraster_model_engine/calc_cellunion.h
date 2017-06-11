#ifndef INCLUDED_CALC_CELLUNION
#define INCLUDED_CALC_CELLUNION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif
// Module headers.


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
