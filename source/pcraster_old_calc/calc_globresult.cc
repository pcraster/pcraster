#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_GLOBRESULT
#include "calc_globresult.h"
#define INCLUDED_CALC_GLOBRESULT
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
#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#include "calc_compressioninput.h"
#define INCLUDED_CALC_COMPRESSIONINPUT
#endif



/*!
  \file
  This file contains the implementation of the GlobResult class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBRESULT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GLOBRESULT MEMBERS
//------------------------------------------------------------------------------

calc::GlobResult::GlobResult(
    VS interfaceVs,
    VS concreteVs,
    const Compressor& c):
         d_concreteVs(concreteVs),
         d_compressor(c)
{
 CSF_CR inCr = biggestCellRepr(d_concreteVs);
  // create the API_MAP with new allocated data
 switch(biggestCellRepr(interfaceVs)) {
  case CR_REAL4:
   d_apiMap = new ApiMapREAL8(d_compressor.rasterSpace(),inCr);
   break;
  case CR_INT4:
   d_apiMap = new  ApiMapINT4(d_compressor.rasterSpace(),inCr);
   break;
  case CR_UINT1:
   d_apiMap = new ApiMapUINT1(d_compressor.rasterSpace(),inCr);
   break;
  default : POSTCOND(FALSE); // NEVER
 }
}



calc::GlobResult::~GlobResult()
{
  delete d_apiMap;
}

void * calc::GlobResult::MAPinterface() const
{
  return d_apiMap->getCPointer();
}

calc::FieldHandle calc::GlobResult::createField() const
{
  CompressionInput ci(d_concreteVs, d_apiMap->detachData(), d_compressor);
  return d_compressor.createSpatial(ci);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
