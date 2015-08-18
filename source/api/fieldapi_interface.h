#ifndef INCLUDED_FIELDAPI_INTERFACE
#define INCLUDED_FIELDAPI_INTERFACE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_READWRITEDATA
#include "fieldapi_readwritedata.h"
#define INCLUDED_FIELDAPI_READWRITEDATA
#endif
#ifndef INCLUDED_FIELDAPI_READONLYSPATIAL
#include "fieldapi_readonlyspatial.h"
#define INCLUDED_FIELDAPI_READONLYSPATIAL
#endif
#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIAL
#include "fieldapi_readonlynonspatial.h"
#define INCLUDED_FIELDAPI_READONLYNONSPATIAL
#endif
#ifndef INCLUDED_API
#include "api.h"
#define INCLUDED_API
#endif



namespace fieldapi {

//! specialization for MAP_* -> ReadOnly
template<class UseAsT, class OldMapApiT>
 ReadOnly<UseAsT> *spatialInterface(const OldMapApiT *o);

//! ReadOnly conversion template, take a MAP_ struct, get a ReadOnly
template<class UseAsT, class OldMapApiT>
 class UpgradeReadOnly {
     //! the new interface
     ReadOnly<UseAsT> *d_ro;
   public:
     UpgradeReadOnly(const OldMapApiT *o);
     ~UpgradeReadOnly();
     const ReadOnly<UseAsT>& readOnly();
};

//! The Read Only Uint1 interface
typedef ReadOnly<UINT1> ReadOnlyUint1;
//! The Read Only Int4 interface
typedef ReadOnly<INT4>  ReadOnlyInt4;
//! The Read Only Real8 interface
typedef ReadOnly<REAL8> ReadOnlyReal8;


//! specialization for MAP_* -> ReadWrite
template<class UseAsT, class OldMapApiT>
 ReadWrite<UseAsT> *spatialInterface( OldMapApiT *o);

//! ReadWrite conversion template, take a MAP_ struct, get a ReadWrite
template<class UseAsT, class OldMapApiT>
 class UpgradeReadWrite {
     //! the new interface
     ReadWrite<UseAsT> *d_ro;
   public:
     UpgradeReadWrite(OldMapApiT *o);
     ~UpgradeReadWrite();
     ReadWrite<UseAsT>& readWrite();
};

//! The Read Write Uint1 interface
typedef ReadWrite<UINT1> ReadWriteUint1;
//! The Read Write Int4 interface
typedef ReadWrite<INT4>  ReadWriteInt4;
//! The Read Write Real8 interface
typedef ReadWrite<REAL8> ReadWriteReal8;

bool nonMV(
    const std::vector<const fieldapi::Common*>& fields,
    const geo::CellLoc& l);

} // namespace fieldapi

//------------------------------------------------------------------------------
// MACRO MAGIC
//------------------------------------------------------------------------------

#define ReadOnlyIMPL_ref(newName,oldName,T,t) \
  fieldapi::UpgradeReadOnly<T,MAP_##T> id_##newName(oldName); \
  const fieldapi::ReadOnly##t& newName(id_##newName.readOnly())

#define ReadOnlyUint1_ref(newName,oldName) \
          ReadOnlyIMPL_ref(newName,oldName,UINT1,Uint1)
#define ReadOnlyInt4_ref(newName,oldName) \
          ReadOnlyIMPL_ref(newName,oldName,INT4,Int4)
#define ReadOnlyReal8_ref(newName,oldName) \
          ReadOnlyIMPL_ref(newName,oldName,REAL8,Real8)

#define ReadWriteIMPL_ref(newName,oldName,T,t) \
  fieldapi::UpgradeReadWrite<T,MAP_##T> id_##newName(oldName); \
  fieldapi::ReadWrite##t& newName(id_##newName.readWrite())

#define ReadWriteUint1_ref(newName,oldName) \
          ReadWriteIMPL_ref(newName,oldName,UINT1,Uint1)
#define ReadWriteInt4_ref(newName,oldName) \
          ReadWriteIMPL_ref(newName,oldName,INT4,Int4)
#define ReadWriteReal8_ref(newName,oldName) \
          ReadWriteIMPL_ref(newName,oldName,REAL8,Real8)


#endif
