#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_READWRITEDATA
#include "fieldapi_readwritedata.h"
#define INCLUDED_FIELDAPI_READWRITEDATA
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the ReadWriteData class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC READWRITEDATA MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF READWRITEDATA MEMBERS
//------------------------------------------------------------------------------

//! ctor
template<class UseAsT, class StoredAsT>
fieldapi::ReadWriteData<UseAsT,StoredAsT>::ReadWriteData(
    StoredAsT **data,
    size_t nrRows, size_t nrCols):
      ReadWrite<UseAsT>(nrRows,nrCols),
      d_ro(data,nrRows,nrCols)
{
}

//! dtor
template<class UseAsT, class StoredAsT>
fieldapi::ReadWriteData<UseAsT,StoredAsT>::~ReadWriteData()
{
}

template<class UseAsT, class StoredAsT>
 void fieldapi::ReadWriteData<UseAsT,StoredAsT>::put(
  UseAsT value, size_t rowIndex, size_t colIndex)
{
#ifdef DEBUG_DEVELOP
  PRECOND(!d_ro.outOfRange(rowIndex,colIndex));
  PRECOND(!pcr::isMV(value));
#endif
  //! down cast with possible loss of precision
  d_ro.d_data[rowIndex][colIndex] = static_cast<StoredAsT>(value);
}

template<class UseAsT, class StoredAsT>
 void fieldapi::ReadWriteData<UseAsT,StoredAsT>::putMV(
  size_t rowIndex, size_t colIndex)
{
#ifdef DEBUG_DEVELOP
  PRECOND(!d_ro.outOfRange(rowIndex,colIndex));
#endif
  pcr::setMV(d_ro.d_data[rowIndex][colIndex]);
}

template<class UseAsT, class StoredAsT>
 void fieldapi::ReadWriteData<UseAsT,StoredAsT>::putAllMV()
{
  pcr::setMV(d_ro.d_data[0],this->nrRows()*this->nrCols());
}

template<class UseAsT, class StoredAsT>
  bool fieldapi::ReadWriteData<UseAsT,StoredAsT>::isMV
  (const geo::CellLoc& l) const
{
  return pcr::isMV(d_ro.d_data[l.row()][l.col()]);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



