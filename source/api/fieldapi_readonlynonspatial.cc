#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIAL
#include "fieldapi_readonlynonspatial.h"
#define INCLUDED_FIELDAPI_READONLYNONSPATIAL
#endif


// Library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// PCRaster library headers.

// Module headers.


/*!
  \file
  This file contains the implementation of the ReadOnlyNonSpatial class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC NONSPATIAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF NONSPATIAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
template<class UseAsT>
 fieldapi::ReadOnlyNonSpatial<UseAsT>::ReadOnlyNonSpatial(
     UseAsT value,
     size_t nrRows,size_t nrCols):
       ReadOnly<UseAsT>(nrRows,nrCols),
       d_value(value)
{
}

//! dtor
template<class UseAsT>
 fieldapi::ReadOnlyNonSpatial<UseAsT>::~ReadOnlyNonSpatial()
{
}


template<class UseAsT>
 bool fieldapi::ReadOnlyNonSpatial<UseAsT>::get(
     UseAsT& value, int rowIndex, int colIndex) const
{
  if (this->outOfRange(rowIndex,colIndex))
    return false;
  value = d_value;
  return true;
}

template<class UseAsT>
 bool fieldapi::ReadOnlyNonSpatial<UseAsT>::get(
     UseAsT& value, size_t rowIndex, size_t colIndex) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(!this->outOfRange(rowIndex,colIndex));
#endif
  (void)rowIndex; // Shut up compiler
  (void)colIndex; // Shut up compiler
  value = d_value;
  return true;
}

/*!
  \todo
     maybe remove develop check on MV, for UINT1 and
     INT4 it might be handy to get MV back as result
 */
template<class UseAsT>
 UseAsT fieldapi::ReadOnlyNonSpatial<UseAsT>::value(
     size_t rowIndex, size_t colIndex) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(!this->outOfRange(rowIndex,colIndex));
  PRECOND(!pcr::isMV(d_value));
#endif
  (void)rowIndex; // Shut up compiler
  (void)colIndex; // Shut up compiler
  return d_value;
}

template<class UseAsT>
 bool fieldapi::ReadOnlyNonSpatial<UseAsT>::spatial() const
{
  return false;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



