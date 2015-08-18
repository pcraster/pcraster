#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READWRITE
#include "fieldapi_readwrite.h"
#define INCLUDED_FIELDAPI_READWRITE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_FIELDAPI_READONLY
#include "fieldapi_readonly.h"
#define INCLUDED_FIELDAPI_READONLY
#endif



/*!
  \file
  This file contains the implementation of the ReadWrite class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC READWRITE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF READWRITE MEMBERS
//------------------------------------------------------------------------------

//! ctor
template<class UseAsT>
 fieldapi::ReadWrite<UseAsT>::ReadWrite(size_t nrRows, size_t nrCols):
  Common(nrRows,nrCols)
{
}

//! dtor
template<class UseAsT>
 fieldapi::ReadWrite<UseAsT>::~ReadWrite()
{
}

template<class UseAsT>
 void fieldapi::ReadWrite<UseAsT>::copy(
   const ReadOnly<UseAsT>& src, const geo::CellLoc& l)
{
  UseAsT v;
  if (src.get(v,l))
     put(v,l);
  else
     putMV(l);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



