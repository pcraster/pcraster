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
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
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
    const FieldHandle field,
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
