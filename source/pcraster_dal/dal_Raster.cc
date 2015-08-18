#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



/*!
  \file
  This file contains the implementation of the Raster class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTER MEMBERS
//------------------------------------------------------------------------------

dal::Raster::Raster(
         DatasetType datasetType,
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north,
         TypeId typeId)

  : Matrix(datasetType, nrRows, nrCols, typeId),
    _dimensions(nrRows, nrCols, cellSize, west, north)

{
}



//! Constructor.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of cols.
  \param     cellSize Cell size.
  \param     west Western-most x-coordinate.
  \param     north Northern-most y-coordinate.
  \param     typeId Type id of cell values.
*/
dal::Raster::Raster(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north,
         TypeId typeId)

  : Matrix(RASTER, nrRows, nrCols, typeId),
    _dimensions(nrRows, nrCols, cellSize, west, north)

{
}



dal::Raster::Raster(
         RasterDimensions const& dimensions,
         TypeId typeId)

  : Matrix(RASTER, dimensions.nrRows(), dimensions.nrCols(), typeId),
    _dimensions(dimensions)

{
}



//! Copy constructor.
dal::Raster::Raster(Raster const& rhs)

  : Matrix(rhs),
    _dimensions(rhs._dimensions)
{
}



//! Destructor.
/*!
*/
dal::Raster::~Raster()
{
}



//! Assignment operator.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
dal::Raster& dal::Raster::operator=(Raster const& rhs)
{
  if(this != &rhs) {
    static_cast<Matrix&>(*this) = rhs;
    _dimensions = rhs._dimensions;
  }

  return *this;
}


//! Returns the cell size.
/*!
  \return    Cell size.
*/
double dal::Raster::cellSize() const
{
  return _dimensions.cellSize();
}



//! Returns the northern-most y-coordinate.
/*!
  \return    Northern-most y-coordinate.

  Top side of the top cell, the cell with a row index of 0.
*/
double dal::Raster::north() const
{
  return _dimensions.north();
}



//! Returns the southern-most y-coordinate.
/*!
  \return    Southern-most y-coordinate.

  Bottom side of the bottom cell, the cell with a row index of nrRows()-1.
*/
double dal::Raster::south() const
{
  return _dimensions.south();
}



//! Returns the western-most x-coordinate.
/*!
  \return    Western-most x-coordinate.

  Left side of the left cell, the cell with a col index of 0.
*/
double dal::Raster::west() const
{
  return _dimensions.west();
}



//! Returns the eastern-most x-coordinate.
/*!
  \return    Eastern-most x-coordinate.

  Right side of the right cell, the cell with a col index of nrCols()-1.
*/
double dal::Raster::east() const
{
  return _dimensions.east();
}



dal::RasterDimensions const& dal::Raster::dimensions() const
{
  return _dimensions;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



