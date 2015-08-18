#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_VALUEBUFFER
#include "calc_valuebuffer.h"
#define INCLUDED_CALC_VALUEBUFFER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ValueBuffer class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC VALUEBUFFER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VALUEBUFFER MEMBERS
//------------------------------------------------------------------------------

//! delete value array
void calc::deallocate(ValueBuffer& v) {
     delete [] v.d_UINT1;
     v.d_void=0;
}

//! return ptr to allocated array and set v to 0
void *calc::detach(ValueBuffer& v) 
{
  void *d(v.d_void);
  v.d_void=0;
  return d;
}

calc::ValueBuffer calc::createValueBuffer(CSF_CR cr,size_t len) {
  ValueBuffer v;
  switch(CELLSIZE(cr)) {
   case 1: v.d_UINT1 = new UINT1[len]; break;
   case 4: v.d_INT4  = new  INT4[len]; break;
  }
  return v;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



