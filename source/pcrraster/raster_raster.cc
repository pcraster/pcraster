#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Raster class.
*/



namespace raster {

discr::Raster* create(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)
{
  return new discr::Raster(nrRows, nrCols, cellSize, west, north);
}



template<typename ValueType>
void setMV(
         discr::RasterData<ValueType>& data,
         size_t index)
{
  pcr::setMV(data.cell(index));
}



template
void setMV<UINT1>(
         discr::RasterData<UINT1>&,
         size_t);

template
void setMV<INT4>(
         discr::RasterData<INT4>&,
         size_t);

template
void setMV<REAL4>(
         discr::RasterData<REAL4>&,
         size_t);

} // namespace raster

