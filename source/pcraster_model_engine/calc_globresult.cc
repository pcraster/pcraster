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
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
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
    const SpatialPacking& c):
         d_up(c,concreteVs)
{
 Field *r=d_up.unpacked();
 CSF_CR inCr = biggestCellRepr(concreteVs);
  // create the API_MAP with new allocated data
 switch(biggestCellRepr(interfaceVs)) {
  case CR_REAL4:
   d_apiMap = new ApiMapREAL8(c.rasterDim(),r->dest(),inCr);
   break;
  case CR_INT4:
   d_apiMap = new  ApiMapINT4(c.rasterDim(),r->dest(),inCr);
   break;
  case CR_UINT1:
   d_apiMap = new ApiMapUINT1(c.rasterDim(),r->dest(),inCr);
   break;
  default : POSTCOND(FALSE); // NEVER
 }
}



calc::GlobResult::~GlobResult()
{
  delete d_apiMap;
}

void *calc::GlobResult::MAPinterface() const
{
  return d_apiMap->getCPointer();
}

calc::Field* calc::GlobResult::createField()
{
  return d_up.releasePacked();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
