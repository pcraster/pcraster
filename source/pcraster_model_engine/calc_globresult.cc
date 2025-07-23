#include "stddefx.h"
#include "calc_globresult.h"
#include "calc_map2csf.h"  // biggestCellRepr
#include "calc_apimap.h"
#include "calc_spatialpacking.h"
#include "geo_rasterspace.h"
#include "calc_spatial.h"

/*!
  \file
  This file contains the implementation of the GlobResult class.
*/

extern template calc::ApiMapC<MAP_UINT1>::InitMap calc::ApiMapC<MAP_UINT1>::d_init;
extern template calc::ApiMapC<MAP_INT4>::InitMap calc::ApiMapC<MAP_INT4>::d_init;
extern template calc::ApiMapC<MAP_REAL8>::InitMap calc::ApiMapC<MAP_REAL8>::d_init;

extern template calc::ApiMapC<MAP_UINT1>::DeleteInternal calc::ApiMapC<MAP_UINT1>::d_del;
extern template calc::ApiMapC<MAP_INT4>::DeleteInternal calc::ApiMapC<MAP_INT4>::d_del;
extern template calc::ApiMapC<MAP_REAL8>::DeleteInternal calc::ApiMapC<MAP_REAL8>::d_del;

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
