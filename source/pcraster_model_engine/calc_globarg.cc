#include "stddefx.h"
#include "calc_globarg.h"
#include "calc_map2csf.h"  // biggestCellRepr
#include "calc_apimap.h"
#include "calc_spatialpacking.h"
#include "calc_field.h"

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

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBARG MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GLOBARG MEMBERS
//------------------------------------------------------------------------------


calc::GlobArg::GlobArg(
    VS interfaceVs,
    const Field& field,
    const SpatialPacking& sp):
     d_unpackedSrc(sp,&field)
{
 CSF_CR inCr = biggestCellRepr(field.vs());
 const void *src  = d_unpackedSrc.src()->src();
 bool  spatial    = field.isSpatial();
 switch(biggestCellRepr(interfaceVs)) {
  case CR_REAL4:
   d_apiMap = new ApiMapREAL8(sp.rasterDim(),src,spatial,inCr);
   break;
  case CR_INT4:
   d_apiMap = new  ApiMapINT4(sp.rasterDim(),src,spatial,inCr);
   break;
  case CR_UINT1:
   d_apiMap = new ApiMapUINT1(sp.rasterDim(),src,spatial,inCr);
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
