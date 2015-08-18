#include "stddefx.h"

#ifndef INCLUDED_CALC_APIMAP
#include "calc_apimap.h"
#define INCLUDED_CALC_APIMAP
#endif

namespace calc {
 template<> ApiMapUINT1::InitMap ApiMapUINT1::d_init(InitMapUINT1);
 template<>  ApiMapINT4::InitMap ApiMapINT4::d_init(InitMapINT4);
 template<> ApiMapREAL8::InitMap ApiMapREAL8::d_init(InitMapREAL8);

 template<> ApiMapUINT1::DeleteInternal ApiMapUINT1::d_del(DeleteInternalMAP_UINT1);
 template<>  ApiMapINT4::DeleteInternal ApiMapINT4::d_del(DeleteInternalMAP_INT4);
 template<> ApiMapREAL8::DeleteInternal ApiMapREAL8::d_del(DeleteInternalMAP_REAL8);
}
