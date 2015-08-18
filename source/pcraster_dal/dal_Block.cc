#ifndef INCLUDED_DAL_BLOCK
#include "dal_Block.h"
#define INCLUDED_DAL_BLOCK
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Block class.
*/



namespace dal {

//------------------------------------------------------------------------------

/*
class BlockPrivate
{
public:

  BlockPrivate()
  {
  }

  ~BlockPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BLOCK MEMBERS
//------------------------------------------------------------------------------

//! Constructor for block data.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Cell size is set to 1.0, west and north to 0.0.
*/
Block::Block(
         size_t nrRows,
         size_t nrCols,
         TypeId typeId)

  : Raster(BLOCK, nrRows, nrCols, 1.0, 0.0, 0.0, typeId),
    d_baseElevation(0),
    d_voxels(0),
    d_isRegular(false)

{
}



//! Constructor for block discretisations.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Block::Block(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)

  : Raster(BLOCK, nrRows, nrCols, cellSize, west, north, TI_REAL4_VECTOR),
    d_baseElevation(
         new Raster(nrRows, nrCols, cellSize, west, north, TI_REAL4)),
    d_voxels(0),
    d_isRegular(false)

{
}



//! Constructor for block discretisations.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Block::Block(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north,
         Raster* baseElevation)

  : Raster(BLOCK, nrRows, nrCols, cellSize, west, north, TI_REAL4_VECTOR),
    d_baseElevation(baseElevation),
    d_voxels(0),
    d_isRegular(false)

{
}



/* NOT IMPLEMENTED
//! Copy constructor.
Block::Block(
         Block const& rhs)

  : Base(rhs)

{
}
*/



Block::~Block()
{
  delete d_voxels;
  delete d_baseElevation;
}



/* NOT IMPLEMENTED
//! Assignment operator.
Block& Block::operator=(
         Block const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



#ifdef DEBUG_DEVELOP
void Block::checkIntegrity() const
{
  assert(
         // Block contains only discretisation information.
         (d_baseElevation != 0 && d_voxels == 0) ||
         // Block contains only data.
         (d_baseElevation == 0 && d_voxels == 0) ||
         // Block contains discretisation information and data.
         (d_baseElevation == 0 && d_voxels != 0));
}
#endif



void Block::setBaseElevation(
         Raster* elevation)
{
  assert(elevation);
  assert(elevation->typeId() == TI_REAL4);

  delete d_baseElevation;
  d_baseElevation = elevation;
}



void Block::setVoxels(
         Block* voxels)
{
  assert(voxels);
  assert(voxels->typeId() == TI_REAL4_VECTOR);

  delete d_voxels;
  d_voxels = voxels;
}



void Block::setIsRegular(
         bool isRegular)
{
  d_isRegular = isRegular;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Discretisation information is in the raster.
*/
bool Block::containsDiscretisationInfo() const
{
#ifdef DEBUG_DEVELOP
  checkIntegrity();
#endif

  return d_baseElevation != 0 && d_voxels == 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Data is in the raster.
*/
bool Block::containsData() const
{
#ifdef DEBUG_DEVELOP
  checkIntegrity();
#endif

  return d_baseElevation == 0 && d_voxels == 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Data is in the raster, discretisation information is in the layered block.
*/
bool Block::containsDiscretisationInfoAndData() const
{
#ifdef DEBUG_DEVELOP
  checkIntegrity();
#endif

  return d_baseElevation == 0 && d_voxels != 0;
}



Raster const* Block::baseElevation() const
{
  return d_baseElevation;
}



Raster* Block::baseElevation()
{
  return d_baseElevation;
}



Block const* Block::voxels() const
{
  return d_voxels;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   Only useful to call when this block contains discretisation info.
  \sa        .
*/
bool Block::isRegular() const
{
  assert(
         containsDiscretisationInfo() ||
         containsDiscretisationInfoAndData());

  if(containsDiscretisationInfoAndData()) {
    return d_voxels->isRegular();
  }
  else {
    return d_isRegular;
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

