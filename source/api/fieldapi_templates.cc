
#include "fieldapi_readonly.cc"
#include "fieldapi_readwritedata.cc"
#include "fieldapi_readwrite.cc"
#include "fieldapi_readonlyspatial.cc"
#include "fieldapi_readonlynonspatial.cc"

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

template class fieldapi::ReadOnly<REAL8>;
template class fieldapi::ReadOnly<INT4>;
template class fieldapi::ReadOnly<UINT1>;

template class fieldapi::ReadWrite<REAL8>;
template class fieldapi::ReadWrite<INT4>;
template class fieldapi::ReadWrite<UINT1>;

template class fieldapi::ReadOnlyNonSpatial<REAL8>;
template class fieldapi::ReadOnlyNonSpatial<INT4>;
template class fieldapi::ReadOnlyNonSpatial<UINT1>;

template class fieldapi::ReadWriteData<REAL8, UINT1>;
template class fieldapi::ReadWriteData<REAL8, INT4>;
template class fieldapi::ReadWriteData<REAL8, REAL4>;

template class fieldapi::ReadWriteData<INT4,UINT1>;
template class fieldapi::ReadWriteData<INT4, INT4>;

template class fieldapi::ReadWriteData<UINT1, UINT1>;

template class fieldapi::ReadOnlySpatial<REAL8, UINT1>;
template class fieldapi::ReadOnlySpatial<REAL8, INT4>;
template class fieldapi::ReadOnlySpatial<REAL8, REAL4>;

template class fieldapi::ReadOnlySpatial<INT4,UINT1>;
template class fieldapi::ReadOnlySpatial<INT4, INT4>;

template class fieldapi::ReadOnlySpatial<UINT1, UINT1>;
