#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_GLOBARG
#include "calc_globarg.h"
#define INCLUDED_CALC_GLOBARG
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"  // biggestCellRepr
#define INCLUDED_CALC_MAP2CSF
#endif
#ifndef INCLUDED_CALC_APIMAP
#include "calc_apimap.h"
#define INCLUDED_CALC_APIMAP
#endif
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

/*!
  \file
  This file contains the implementation of the GlobArg class.
*/


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
