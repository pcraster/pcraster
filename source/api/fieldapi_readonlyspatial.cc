#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READONLYSPATIAL
#include "fieldapi_readonlyspatial.h"
#define INCLUDED_FIELDAPI_READONLYSPATIAL
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
  This file contains the implementation of the ReadOnlySpatial class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPATIAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPATIAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
template<class UseAsT, class StoredAsT>
fieldapi::ReadOnlySpatial<UseAsT,StoredAsT>::ReadOnlySpatial(
     StoredAsT **data,
     size_t nrRows,size_t nrCols):
      ReadOnly<UseAsT>(nrRows,nrCols),
      d_data(data)
{
}

//! dtor
template<class UseAsT, class StoredAsT>
fieldapi::ReadOnlySpatial<UseAsT,StoredAsT>::~ReadOnlySpatial()
{
}

template<class UseAsT, class StoredAsT>
 bool fieldapi::ReadOnlySpatial<UseAsT,StoredAsT>::get(
     UseAsT& value, int rowIndex, int colIndex) const
{
  if (this->outOfRange(rowIndex,colIndex))
    return false;
  if (pcr::isMV(d_data[rowIndex][colIndex]))
    return false;
  value = d_data[rowIndex][colIndex];
  return true;
}

template<class UseAsT, class StoredAsT>
 bool fieldapi::ReadOnlySpatial<UseAsT,StoredAsT>::get(
     UseAsT& value, size_t rowIndex, size_t colIndex) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(!this->outOfRange(rowIndex,colIndex));
#endif
  if (pcr::isMV(d_data[rowIndex][colIndex]))
    return false;
  value = d_data[rowIndex][colIndex];
  return true;
}

/*!
  \todo
     maybe remove develop check on MV, for UINT1 and
     INT4 it might be handy to get MV back as result
 */
template<class UseAsT, class StoredAsT>
 UseAsT fieldapi::ReadOnlySpatial<UseAsT,StoredAsT>::value(
     size_t rowIndex, size_t colIndex) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(!this->outOfRange(rowIndex,colIndex));
  PRECOND(!pcr::isMV(d_data[rowIndex][colIndex]));
#endif
  return d_data[rowIndex][colIndex];
}


template<class UseAsT, class StoredAsT>
 bool fieldapi::ReadOnlySpatial<UseAsT,StoredAsT>::spatial() const
{
  return true;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
