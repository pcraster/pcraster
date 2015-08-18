#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif

// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif



/*!
  \file
  This file contains the implementation of the RasterDimensions class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDIMENSIONS MEMBERS
//------------------------------------------------------------------------------

//! Determines raster dimensions of overlapping area of two raster dimensions.
/*!
  \param     dimensions1 Dimensional properties of first raster.
  \param     dimensions2 Dimensional properties of second raster.
  \return    Tuple containing dimensional properties of the overlapping area
             of the two rasters passed in.
  \sa        RasterDimensions::areaDimenstions(RasterDimensions const&, double, double, double, double)

  The idea is to find the overlapping areas of two rasters with, possibly,
  different properties (cell sizes, nrRows/nrCols, etc). The objects returned
  contain the cells that arepartly or totally covered by the other raster.
  An object is returned for each raster whose dimensional properties are passed
  in, in the same order. So the first object of the tuple returned corresponds
  to the first dimensions object passed in, etc.

  This information can be used for implementing a resampling algorithm, for
  example. In case of resmpling, the information returned points to the cells
  that need to be handled, while all other cells in the target raster
  can be set to MV.
*/
boost::tuple<RasterDimensions, RasterDimensions> RasterDimensions::overlap(
         RasterDimensions const& dimensions1,
         RasterDimensions const& dimensions2)
{
  // Determine overlapping area in world coordinates.
  double west  = std::max(dimensions1.west(),  dimensions2.west());
  double east  = std::min(dimensions1.east(),  dimensions2.east());
  double north = std::min(dimensions1.north(), dimensions2.north());
  double south = std::max(dimensions1.south(), dimensions2.south());

  return boost::make_tuple(
         dimensions1.areaDimensions(west, north, east, south),
         dimensions2.areaDimensions(west, north, east, south));
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDIMENSIONS MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  The default cell size is 1.0.
*/
RasterDimensions::RasterDimensions()

  : MatrixDimensions(),
    SpaceDimensions(),
    _cellSize(1.0)

{
}



//! Constructor.
/*!
  \param     nrRows Number of rows in the raster.
  \param     nrCols Number of columns in the raster.
  \param     cellSize Size of cells.
  \param     west Western most coordinate.
  \param     north Northern most coordinate.
  \todo      Make cell size the last argument.
  \warning   The \a cellSize passed in must be larger than 0.
*/
RasterDimensions::RasterDimensions(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)

  : MatrixDimensions(nrRows, nrCols),
    SpaceDimensions(west, north, west + (cellSize * nrCols),
         north - (cellSize * nrRows)),
    _cellSize(cellSize)

{
  assert(_cellSize > 0.0);
}



//! Constructor.
/*!
  \param     nrRows Number of rows in the raster.
  \param     nrCols Number of columns in the raster.
  \param     cellSize Size of cells.
  \param     northWest North-west coordinate of raster.
  \todo      Make cell size the last argument.
  \warning   The \a cellSize passed in must be larger than 0.
*/
RasterDimensions::RasterDimensions(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         SpatialCoordinate const& northWest)

  : MatrixDimensions(nrRows, nrCols),
    SpaceDimensions(northWest, SpatialCoordinate(
         northWest.x() + (cellSize * nrCols),
         northWest.y() - (cellSize * nrRows))),
    _cellSize(cellSize)

{
  assert(_cellSize > 0.0);
}



//! Copy constructor.
/*!
  \param     rhs Object to copy from.
*/
RasterDimensions::RasterDimensions(
         RasterDimensions const& rhs)

  : MatrixDimensions(rhs),
    SpaceDimensions(rhs),
    _cellSize(rhs._cellSize)

{
}



//! Assignment operator.
/*!
  \param     rhs Object to copy from.
  \return    Reference to *this.
*/
RasterDimensions const& RasterDimensions::operator=(
         RasterDimensions const& rhs)
{
  if(this != &rhs) {
    MatrixDimensions::operator=(rhs);
    SpaceDimensions::operator=(rhs);
    _cellSize = rhs._cellSize;
  }

  return *this;
}



//! Combines / merges \a rhs with *this.
/*!
  \param     rhs Raster to merge with *this.
  \return    Updated raster.
  \warning   The result of dividing the latitudinal extent and longitudinal
             extent by the smallest cell size, must be an integral value.
             These are the number of rows and cols of the merged raster.
             Not all two rasters can be merged.
  \sa        compatible(RasterDimensions const&)
*/
RasterDimensions const& RasterDimensions::operator|=(
         RasterDimensions const& rhs)
{
  assert(compatible(rhs));

  if(this != &rhs) {
    SpaceDimensions::operator|=(rhs);
    _cellSize = std::min(_cellSize, rhs._cellSize);

    double nrRows = latitudinalExtent() / _cellSize;
    double nrCols = longitudinalExtent() / _cellSize;

    assert(comparable(fmod(nrRows, 1.0), 0.0));
    assert(comparable(fmod(nrCols, 1.0), 0.0));

    setNrRows(static_cast<size_t>(nrRows));
    setNrCols(static_cast<size_t>(nrCols));
  }

  return *this;
}



//! Destructor.
/*!
*/
RasterDimensions::~RasterDimensions()
{
}



//! Returns whether \a rhs equals *this.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
bool RasterDimensions::equals(
         RasterDimensions const& rhs) const
{
  return dynamic_cast<MatrixDimensions const&>(*this) == rhs &&
         dynamic_cast<SpaceDimensions const&>(*this) == rhs &&
         dal::comparable(_cellSize, rhs._cellSize);
}



//! Returns the cell size.
/*!
  \return    Cell size.
*/
double RasterDimensions::cellSize() const
{
  return _cellSize;
}



//! Returns the one-dimensions equivalent of the two-dimensional indices passed in.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Index.
  \warning   The \a row and \a col indices must be smaller than the number of
             rows and cols, respectively.

  Handy for translating row and col indices to array index.
*/
size_t RasterDimensions::index(
         size_t row,
         size_t col) const
{
  assert(row < nrRows());
  assert(col < nrCols());

  return (row * nrCols()) + col;
}



//! Returns the linear index of the cell containing \a x, \a y.
/*!
  \param     x X-coordinate of cell.
  \param     y Y-coordinate of cell.
  \return    Linear index of cell.
  \warning   The cell containing \a x, \a y must be part of the raster.
*/
size_t RasterDimensions::index(
         double x,
         double y) const
{
  double row, col;

  indices(x, y, row, col);

  assert(containsCell(row, col));

  return index(static_cast<size_t>(row), static_cast<size_t>(col));
}



//! Sets the \a row and \a col index, given \a x and \a y coordinates.
/*!
  \param     x X coordinate.
  \param     y Y coordinate.
  \param     row Row index.
  \param     col Column index.
  \return    The \a row and \a col indices are updated.
  \todo      Test.
*/
void RasterDimensions::indices(
         double x,
         double y,
         double& row,
         double& col) const
{
  row = (north() - y) / _cellSize;
  col = (x - west()) / _cellSize;
}



//! Sets the \a row and \a col index, given the \a coordinate.
/*!
  \param     coordinate Coordinate.
  \param     row Row index.
  \param     col Column index.
  \return    The \a row and \a col indices are updated.
  \todo      Test.
*/
void RasterDimensions::indices(
         SpatialCoordinate const& coordinate,
         double& row,
         double& col) const
{
  indices(coordinate.x(), coordinate.y(), row, col);
}



//! Convert area in world coordinates to cell indices with similar properties as *this.
/*!
  \param     west World coordinate of west of area.
  \param     north World coordinate of north of area.
  \param     east World coordinate of east of area.
  \param     south World coordinate of south of area.
  \return    Dimensional properties of raster of area passed in.
  \sa        boost::tuple<RasterDimensions, RasterDimensions> RasterDimensions::overlap(RasterDimensions const&, RasterDimensions const&)

  In case the resulting raster dimension is empty, the object returned will
  have the same properties as *this, except for the number of rows and columns,
  which will be set to zero.
*/
RasterDimensions RasterDimensions::areaDimensions(
         double west,
         double north,
         double east,
         double south) const
{
  RasterDimensions result(size_t(0), size_t(0), this->cellSize(),
         this->west(), this->north());

  if(west < east && north > south) {
    // Determine cell-indices of corners of overlap.
    double westIndex, northIndex, eastIndex, southIndex;
    this->indices(west, north, northIndex, westIndex);
    this->indices(east, south, southIndex, eastIndex);

    assert(westIndex >= 0.0);
    assert(eastIndex >= 0.0);
    assert(northIndex >= 0.0);
    assert(southIndex >= 0.0);
    assert(westIndex <= eastIndex);
    assert(northIndex <= southIndex);

    // Determine coordinates of north-west corner of north-west cell.
    double northSnapped, westSnapped;
    this->coordinates(
         static_cast<size_t>(std::floor(northIndex)),
         static_cast<size_t>(std::floor(westIndex)),
         westSnapped, northSnapped);

    // Aggregate results in RasterDimensions instance.
    result = RasterDimensions(
         static_cast<size_t>(std::ceil(southIndex - northIndex)),
         static_cast<size_t>(std::ceil(eastIndex - westIndex)),
         this->cellSize(), westSnapped, northSnapped);
  }

  return result;
}



//! Returns whether the raster contains cell \a row, \a col.
/*!
  \param     row Row index of cell.
  \param     col Col index of cell.
  \return    true or false

  The south and east border of a cell are not considered part of the cell.
  So, for a 1x1 raster, (0, 0) is part of the raster, but (0, 1), (1, 0) and
  (1, 1) are not.
*/
bool RasterDimensions::containsCell(
         double row,
         double col) const
{
  return greaterOrComparable(row, 0.0) && row < static_cast<double>(nrRows()) &&
         greaterOrComparable(col, 0.0) && col < static_cast<double>(nrCols());
}



//! Sets the \a x and \y coordinates, given the linear cell \a index.
/*!
  \param     index Linear cell index.
  \param     x X coordinate to set.
  \param     y Y coordinate to set.
  \return    The \a x and \a y coordinates are updated.

  The coordinates of the center of the cell are set.
*/
void RasterDimensions::coordinates(
         size_t index,
         double& x,
         double& y) const
{
  double row = index / nrCols();
  double col = index % nrCols();

  coordinates(row + 0.5, col + 0.5, x, y);
}



//! Returns the \a x and \a y coordinates, given \a row and \a col indices.
/*!
  \param     row Row index.
  \param     col Col index.
  \param     x X coordinate.
  \param     y Y coordinate.
  \return    The \a x and \a y coordinates are updated.
  \todo      Test.

  If \a row and/or \a col is a whole number, than the coordinate of the western
  most and/or northern most side of the cell is set.
*/
void RasterDimensions::coordinates(
         double row,
         double col,
         double& x,
         double& y) const
{
  y = north() - (_cellSize * row);
  x = west() + (_cellSize * col);
}



//! Returns whether \a rhs is compatible with *this.
/*!
  \param     rhs Raster to test.
  \return    true or false
  \exception .
  \warning   .
  \sa        .
  \todo      Check the code with the code in
             operator|=(RasterDimensions const&). Should be the same.
*/
bool RasterDimensions::compatible(
         RasterDimensions const& rhs) const
{
  double cellSize = std::min(_cellSize, rhs._cellSize);

  SpaceDimensions envelope(*this);
  envelope |= rhs;

  return comparable(fmod(west() - envelope.west(), cellSize), 0.0) &&
         comparable(fmod(envelope.north() - north(), cellSize), 0.0) &&
         comparable(fmod(envelope.east() - east(), cellSize), 0.0) &&
         comparable(fmod(south() - envelope.south(), cellSize), 0.0);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Equality operator.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
PCR_DAL_DECL bool operator==(
         RasterDimensions const& lhs,
         RasterDimensions const& rhs)
{
  return lhs.equals(rhs);
}



//! Inequality operator.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
PCR_DAL_DECL bool operator!=(
         RasterDimensions const& lhs,
         RasterDimensions const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

