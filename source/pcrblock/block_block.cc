#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Block class.
*/



namespace block {

//! Creates a new block with the given \a baseElevation.
/*!
  \param     baseElevation Base elevation of the new block.
  \return    block
  \warning   Caller is responsible for deleting the created block.
*/
discr::Block* create(
         discr::RasterData<REAL4> const* baseElevation)
{
  return new discr::Block(*baseElevation);
}



//! Determines the base elevation of each cell and assigns it to \a result.
/*!
  \param     result Resulting elevation.
  \param     block Block to analyse.
*/
void baseElevation(
         discr::RasterData<REAL4>& result,
         discr::Block const& block)
{
  DEVELOP_PRECOND(
         result.raster() == &static_cast<discr::Raster const&>(block) ||
         *result.raster() == static_cast<discr::Raster const&>(block));

  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(block.isMV(i)) {
      pcr::setMV(result.cell(i));
    }
    else {
      result.cell(i) = block.cell(i).baseElevation();
    }
  }
}



//! Determines the surface elevation of each cell and assigns it to \a result.
/*!
  \param     result Resulting elevation.
  \param     block Block to analyse.
*/
void surfaceElevation(
         discr::RasterData<REAL4>& result,
         discr::Block const& block)
{
  DEVELOP_PRECOND(
         result.raster() == &static_cast<discr::Raster const&>(block) ||
         *result.raster() == static_cast<discr::Raster const&>(block));

  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(block.isMV(i)) {
      pcr::setMV(result.cell(i));
    }
    else {
      result.cell(i) = block.cell(i).surfaceElevation();
    }
  }
}



//! Sets the default value of \a result to \a value.
/*!
  \param     result Data which default value is changed.
  \param     value Default value to set.
*/
template<typename T>
void setDefaultValue(
         discr::BlockData<T>& result,
         discr::RasterData<T> const& value)
{
  result.setDefaultValue(value);
}

template
void setDefaultValue(
         discr::BlockData<UINT1>&,
         discr::RasterData<UINT1> const&);
template
void setDefaultValue(
         discr::BlockData<INT4>&,
         discr::RasterData<INT4> const&);
template
void setDefaultValue(
         discr::BlockData<REAL4>&,
         discr::RasterData<REAL4> const&);

} // namespace block

