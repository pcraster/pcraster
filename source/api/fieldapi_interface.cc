#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the interface class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTERFACE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF INTERFACE MEMBERS
//------------------------------------------------------------------------------

namespace fieldapi {

template <>
 ReadOnlyUint1 *spatialInterface<UINT1>(const MAP_UINT1 *o)
{
  if(o->inCellRepr == CR_UINT1)
    return new ReadOnlySpatial<UINT1,UINT1>
                ((UINT1 **)o->spatialValue, o->nrRows,o->nrCols);
  PRECOND(FALSE);
  return 0;
}

template<>
 ReadOnlyInt4 *spatialInterface<INT4,MAP_INT4>(const MAP_INT4 *o)
{
  switch(o->inCellRepr) {
    case CR_INT4: return new ReadOnlySpatial<INT4,INT4>
          ((INT4 **)o->spatialValue, o->nrRows,o->nrCols);
    case CR_UINT1: return new ReadOnlySpatial<INT4,UINT1>
          ((UINT1 **)o->spatialValue, o->nrRows,o->nrCols);
    default:
          ;
  }
  PRECOND(FALSE);
  return 0;
}

template<>
 ReadOnlyReal8 *spatialInterface<REAL8,MAP_REAL8>(const MAP_REAL8 *o)
{
  switch(o->inCellRepr) {
    case CR_REAL4: return new ReadOnlySpatial<REAL8,REAL4>
          ((REAL4 **)o->spatialValue, o->nrRows,o->nrCols);
    case CR_INT4: return new ReadOnlySpatial<REAL8,INT4>
          ((INT4 **)o->spatialValue, o->nrRows,o->nrCols);
    case CR_UINT1: return new ReadOnlySpatial<REAL8,UINT1>
          ((UINT1 **)o->spatialValue, o->nrRows,o->nrCols);
    default:
          ;
  }
  PRECOND(FALSE);
  return 0;
}


//! ReadOnly conversion template, take a MAP_ struct, get a ReadOnly
template<class UseAsT,class OldMapApiT >
 fieldapi::UpgradeReadOnly<UseAsT,OldMapApiT>::UpgradeReadOnly(const OldMapApiT *o)
{
   if (o->spatial)
     d_ro = spatialInterface<UseAsT,OldMapApiT>(o);
   else
     d_ro = new ReadOnlyNonSpatial<UseAsT>(
                 o->nonSpatialValue, o->nrRows,o->nrCols);
}

//! dtor
template<class UseAsT, class OldMapApiT>
 fieldapi::UpgradeReadOnly<UseAsT,OldMapApiT>::~UpgradeReadOnly()
{
  delete d_ro;
}

//! return new interface
template<class UseAsT, class OldMapApiT>
 const fieldapi::ReadOnly<UseAsT>&
 fieldapi::UpgradeReadOnly<UseAsT,OldMapApiT>::readOnly()
{
  return *d_ro;
}

template class fieldapi::UpgradeReadOnly<UINT1,MAP_UINT1>;
template class fieldapi::UpgradeReadOnly<INT4, MAP_INT4>;
template class fieldapi::UpgradeReadOnly<REAL8,MAP_REAL8>;

template <>
 ReadWriteUint1
 *spatialInterface<UINT1>(MAP_UINT1 *o)
{
  if(o->inCellRepr == CR_UINT1)
    return new ReadWriteData<UINT1,UINT1>
                ((UINT1 **)o->spatialValue, o->nrRows,o->nrCols);
  PRECOND(FALSE);
  return 0;
}

template<>
 ReadWriteInt4
 *spatialInterface<INT4,MAP_INT4>(MAP_INT4 *o)
{
  switch(o->inCellRepr) {
    case CR_INT4: return new ReadWriteData<INT4,INT4>
          ((INT4 **)o->spatialValue, o->nrRows,o->nrCols);
    case CR_UINT1: return new ReadWriteData<INT4,UINT1>
          ((UINT1 **)o->spatialValue, o->nrRows,o->nrCols);
    default:
          ;
  }
  PRECOND(FALSE);
  return 0;
}

template<>
 ReadWriteReal8
 *spatialInterface<REAL8,MAP_REAL8>(MAP_REAL8 *o)
{
  switch(o->inCellRepr) {
    case CR_REAL4: return new ReadWriteData<REAL8,REAL4>
          ((REAL4 **)o->spatialValue, o->nrRows,o->nrCols);
    case CR_INT4: return new ReadWriteData<REAL8,INT4>
          ((INT4 **)o->spatialValue, o->nrRows,o->nrCols);
    case CR_UINT1: return new ReadWriteData<REAL8,UINT1>
          ((UINT1 **)o->spatialValue, o->nrRows,o->nrCols);
    default:
          ;
  }
  PRECOND(FALSE);
  return 0;
}

//! ReadWrite conversion template, take a MAP_ struct, get a ReadWrite
template<class UseAsT,class OldMapApiT >
 fieldapi::UpgradeReadWrite<UseAsT,OldMapApiT>::UpgradeReadWrite(OldMapApiT *o)
{
   PRECOND(o->spatial);
   d_ro = spatialInterface<UseAsT,OldMapApiT>(o);
}

//! dtor
template<class UseAsT, class OldMapApiT>
 fieldapi::UpgradeReadWrite<UseAsT,OldMapApiT>::~UpgradeReadWrite()
{
  delete d_ro;
}

//! return new interface
template<class UseAsT, class OldMapApiT>
 fieldapi::ReadWrite<UseAsT>&
 fieldapi::UpgradeReadWrite<UseAsT,OldMapApiT>::readWrite()
{
  return *d_ro;
}

template class fieldapi::UpgradeReadWrite<UINT1,MAP_UINT1>;
template class fieldapi::UpgradeReadWrite<INT4, MAP_INT4>;
template class fieldapi::UpgradeReadWrite<REAL8,MAP_REAL8>;

//! test if any of the \a fields has a MV on location \a l
/*!
    \todo
       Make this an object with ctor taking fields, we can then
       optimize for leaving out the non-spatials, they are always
       nonMV
 */
bool nonMV(
    const std::vector<const fieldapi::Common*>& fields,
    const geo::CellLoc& l)
{
  for(size_t i=0; i< fields.size(); i++)
    if (fields[i]->isMV(l))
      return false;
  return true;
}

} // namespace fieldapi
