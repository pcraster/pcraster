#ifndef INCLUDED_CALC_VALUEBUFFER
#define INCLUDED_CALC_VALUEBUFFER



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
  // ValueBuffer declarations.
}



namespace calc {

typedef union ValueBuffer {
   UINT1   *d_UINT1;
   INT4    *d_INT4;
   REAL4   *d_REAL4;
   void    *d_void;
} ValueBuffer;

typedef union ConstValueBuffer {
   const UINT1   *d_UINT1;
   const INT4    *d_INT4;
   const REAL4   *d_REAL4;
   const void    *d_void;
} ConstValueBuffer;


void deallocate(ValueBuffer& v);
void *detach(ValueBuffer& v);

ValueBuffer createValueBuffer(CSF_CR cr,size_t len);

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
