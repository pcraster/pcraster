#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Raster class.
*/



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of columns.
  \param     cellSize Cell size.
  \param     west x-coordinate of left side of western most column.
  \param     north y-coordinate of top side of northern most row.

  Creates a raster discretisation object based on the properties passed in.
*/
Raster::Raster(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)

  : d_nrRows(nrRows), d_nrCols(nrCols), d_cellSize(cellSize),
    d_west(west), d_north(north)

{
}



//! Destructor.
/*!
*/
Raster::~Raster()
{
}



//! Returns the number of rows in the raster.
/*!
  \return    Number of rows.
*/
size_t Raster::nrRows() const
{
  return d_nrRows;
}



//! Returns the number of columns in the raster.
/*!
  \return    Number of columns.
*/
size_t Raster::nrCols() const
{
  return d_nrCols;
}



//! Returns the number of cells in the raster.
/*!
  \return    Number of cells.
*/
size_t Raster::nrCells() const
{
  return d_nrRows * d_nrCols;
}



//! Returns the size of the cells.
/*!
  \return    size
*/
double Raster::cellSize() const
{
  return d_cellSize;
}



//! Returns the x-coordinate of left side of western most column.
/*!
  \return    coordinate
*/
double Raster::west() const
{
  return d_west;
}



//! Returns the y-coordinate of top side of northern most row.
/*!
  \return    coordinate
*/
double Raster::north() const
{
  return d_north;
}



//! Returns whether \a rhs equals *this.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
bool Raster::equals(Raster const& rhs) const
{
  return d_nrRows == rhs.d_nrRows &&
         d_nrCols == rhs.d_nrCols &&
         d_cellSize == rhs.d_cellSize &&
         d_west == rhs.d_west &&
         d_north == rhs.d_north;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Returns whether \a lhs is equal to \a rhs.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
bool operator==(
         Raster const& lhs,
         Raster const& rhs)
{
  return lhs.equals(rhs);
}



//! Returns whether \a lhs is not equal to \a rhs.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
bool operator!=(
         Raster const& lhs,
         Raster const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace discr

