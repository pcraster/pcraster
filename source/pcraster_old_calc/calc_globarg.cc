#include "stddefx.h"
#include "calc_globarg.h"
#include "calc_map2csf.h"  // biggestCellRepr
#include "calc_apimap.h"
#include "calc_compressor.h"
#include "geo_rasterspace.h"
#include "calc_spatial.h"



/*!
  \file
  This file contains the implementation of the GlobArg class.
*/

extern template calc::ApiMapC<MAP_UINT1>::InitMap calc::ApiMapC<MAP_UINT1>::d_init;
extern template calc::ApiMapC<MAP_INT4>::InitMap calc::ApiMapC<MAP_INT4>::d_init;
extern template calc::ApiMapC<MAP_REAL8>::InitMap calc::ApiMapC<MAP_REAL8>::d_init;

extern template calc::ApiMapC<MAP_UINT1>::DeleteInternal calc::ApiMapC<MAP_UINT1>::d_del;
extern template calc::ApiMapC<MAP_INT4>::DeleteInternal calc::ApiMapC<MAP_INT4>::d_del;
extern template calc::ApiMapC<MAP_REAL8>::DeleteInternal calc::ApiMapC<MAP_REAL8>::d_del;

extern template int calc::ObjCount<calc::ApiMap>::numObjects;

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBARG MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GLOBARG MEMBERS
//------------------------------------------------------------------------------

calc::GlobArg::GlobArg(
    VS interfaceVs,
    const FieldHandle& field,
    const Compressor& compressor):
        d_copy(field->vs())
{
 CSF_CR inCr = biggestCellRepr(field->vs());
 const void *data  = field->srcValue();
 if (field->isSpatial()) {
   compressor.decompress(d_copy,data);
   data = d_copy.decompressed();
 }
 switch(biggestCellRepr(interfaceVs)) {
  case CR_REAL4:
   d_apiMap = new ApiMapREAL8(compressor.rasterSpace(),data,field->isSpatial(),inCr);
   break;
  case CR_INT4:
   d_apiMap = new  ApiMapINT4(compressor.rasterSpace(),data,field->isSpatial(),inCr);
   break;
  case CR_UINT1:
   d_apiMap = new ApiMapUINT1(compressor.rasterSpace(),data,field->isSpatial(),inCr);
   break;
  default : POSTCOND(FALSE); // NEVER
 }
}

calc::GlobArg::~GlobArg()
{
  delete d_apiMap;
}

void * calc::GlobArg::MAPinterface() const
{
  return d_apiMap->getCPointer();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
