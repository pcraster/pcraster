#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READONLY
#include "fieldapi_readonly.h"
#define INCLUDED_FIELDAPI_READONLY
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ReadOnly class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC READONLY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF READONLY MEMBERS
//------------------------------------------------------------------------------

//! ctor
template<class UseAsT>
 fieldapi::ReadOnly<UseAsT>::ReadOnly(
       size_t nrRows,size_t nrCols):
  Common(nrRows,nrCols)
{
}

//! dtor
template<class UseAsT>
 fieldapi::ReadOnly<UseAsT>::~ReadOnly()
{
}

template<class UseAsT>
  bool fieldapi::ReadOnly<UseAsT>::isMV(const geo::CellLoc& l) const
{
  UseAsT v;
  return !get(v,l);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



