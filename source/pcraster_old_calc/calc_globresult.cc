#include "stddefx.h"
#include "calc_globresult.h"
#include "calc_map2csf.h"  // biggestCellRepr
#include "calc_apimap.h"
#include "calc_compressor.h"
#include "geo_rasterspace.h"
#include "calc_spatial.h"
#include "calc_compressioninput.h"


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

extern template int calc::ObjCount<calc::ApiMap>::numObjects;

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBRESULT MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF GLOBRESULT MEMBERS
//------------------------------------------------------------------------------

calc::GlobResult::GlobResult(VS interfaceVs, VS concreteVs, const Compressor &c)
    : d_concreteVs(concreteVs), d_compressor(c)
{
  CSF_CR const inCr = biggestCellRepr(d_concreteVs);
  // create the API_MAP with new allocated data
  switch (biggestCellRepr(interfaceVs)) {
    case CR_REAL4:
      d_apiMap = new ApiMapREAL8(d_compressor.rasterSpace(), inCr);
      break;
    case CR_INT4:
      d_apiMap = new ApiMapINT4(d_compressor.rasterSpace(), inCr);
      break;
    case CR_UINT1:
      d_apiMap = new ApiMapUINT1(d_compressor.rasterSpace(), inCr);
      break;
    default:
      POSTCOND(false);  // NEVER
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
