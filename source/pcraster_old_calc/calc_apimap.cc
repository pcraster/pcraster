#include "stddefx.h"
#include "calc_apimap.h"

namespace calc {
 template<> int ObjCount<ApiMap>::numObjects(0);
 static ObjCounter<ApiMap> apiMapCounter("calc::ApiMap");


 template<> ApiMapUINT1::InitMap ApiMapUINT1::d_init(InitMapUINT1);
 template<>  ApiMapINT4::InitMap ApiMapINT4::d_init(InitMapINT4);
 template<> ApiMapREAL8::InitMap ApiMapREAL8::d_init(InitMapREAL8);

 template<> ApiMapUINT1::DeleteInternal ApiMapUINT1::d_del(DeleteInternalMAP_UINT1);
 template<>  ApiMapINT4::DeleteInternal ApiMapINT4::d_del(DeleteInternalMAP_INT4);
 template<> ApiMapREAL8::DeleteInternal ApiMapREAL8::d_del(DeleteInternalMAP_REAL8);
}

void* calc::ApiMap::allocate(CSF_CR inCr, size_t nrCells)
{
   return (void *)new char[CSFSIZEOF(nrCells,inCr)];
}
