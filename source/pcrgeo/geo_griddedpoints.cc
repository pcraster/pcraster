#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_GRIDDEDPOINTS
#include "geo_griddedpoints.h"
#define INCLUDED_GEO_GRIDDEDPOINTS
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#include "geo_circularneighbourhood.h"
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_GEO_SCANCONVERSION
#include "geo_scanconversion.h"
#define INCLUDED_GEO_SCANCONVERSION
#endif



/*!
  \file
  This file contains the implementation of the GriddedPoints class.
*/


namespace geo {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GRIDDEDPOINTS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GRIDDEDPOINTS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     space Spatial properties of the raster.
  \exception .
  \warning   .
  \sa        .

  All cell are initialized as non-missing value, empty cells.
*/
template<class Point>
GriddedPoints<Point>::GriddedPoints(const RasterSpace& space)

  : d_space(space), d_points(space.nrRows(), space.nrCols()), 
    d_missingValues(space.nrRows(), space.nrCols(), false)

{
  PRECOND((space.nrRows() > 0) && (space.nrCols() > 0));
}



template<class Point>
GriddedPoints<Point>::GriddedPoints(const RasterSpace& space,
         const MVRaster& missingValues)

  : d_space(space), d_points(space.nrRows(), space.nrCols()),
    d_missingValues(missingValues)

{
  PRECOND((space.nrRows() > 0) && (space.nrCols() > 0));
}



template<class Point>
GriddedPoints<Point>::GriddedPoints(const GriddedPoints& rhs)

  : d_space(rhs.d_space),
    d_points(static_cast<RasterDim const&>(rhs.d_points)),
    d_missingValues(rhs.d_missingValues)

{
  d_points = rhs.d_points;
}



//! dtor
template<class Point>
GriddedPoints<Point>::~GriddedPoints()
{
}



//! Sets cell \a row, \a col to a missing value.
/*!
  \param     row Row index.
  \param     col Column index.
  \warning   All points in this cell are removed.
*/
template<class Point>
void GriddedPoints<Point>::setMV(size_t row, size_t col)
{
  d_points.cell(row, col).clear();
  d_missingValues.cell(row, col) = true;
}



template<class Point>
void GriddedPoints<Point>::setMV(const CellLoc& loc)
{
  d_points.cell(loc.row(), loc.col()).clear();
  d_missingValues.cell(loc.row(), loc.col()) = true;
}



//! Inserts point \a point in a grid cell.
/*!
  \param     point Point to insert.
  \return    .
  \exception .
  \warning   The coordinates of \a point must correspond to a cell in the raster. This cell must not be a missing value cell.
  \sa        .
*/
template<class Point>
typename GriddedPoints<Point>::iterator
         GriddedPoints<Point>::insert(const Point& point)
{
/*
 *double r, c;

 *d_space.coords2RowCol(point[0], point[1], r, c);

 *PRECOND(r >= 0.0);
 *PRECOND(c >= 0.0);

 *size_t row = static_cast<size_t>(r);
 *size_t col = static_cast<size_t>(c);

 *PRECOND(row < nrRows());
 *PRECOND(col < nrCols());
 *PRECOND(!isMV(row, col));

 *d_points.cell(row, col).push_back(point);
 */

  CellLoc loc = cellLoc<Point>(point);
  PRECOND(!isMV(loc));
  d_points.cell(loc).push_back(point);
  return --d_points.cell(loc).end();
}



//! Removes all points from the raster. Makes each raster cell empty.
/*!
*/
template<class Point>
void GriddedPoints<Point>::clear()
{
  std::for_each(d_points.begin(), d_points.end(),
         std::mem_fun_ref(&List::clear));
}



//! Removes point pointed to by \a it from cell \a row, \a col.
/*!
  \param     it Iterator to point.
  \param     row Row index.
  \param     col Column index.
  \exception .
  \warning   The cell at position \a row, \a col must not be a missing value. This cell must contain the point pointed to by \a it.
  \sa        .
  \todo      check if precondition is still needed.
*/
template<class Point>
void GriddedPoints<Point>::remove(iterator it, size_t row, size_t col)
{
  PRECOND(!isMV(row, col));
  PRECOND(std::find(begin(row, col), end(row, col), *it) != end(row, col));

  d_points.cell(row, col).erase(it);
}



template<class Point>
void GriddedPoints<Point>::remove(const Point& point, size_t row,
         size_t col)
{
  PRECOND(!isMV(row, col));

  iterator it = std::find(begin(row, col), end(row, col), point);
  POSTCOND(it != end(row, col));

  d_points.cell(row, col).erase(it);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  This function assumes that the co-ordinates of the point in *it have changed.

*/
template<class Point>
void GriddedPoints<Point>::move(iterator it, size_t row, size_t col)
{
  Point point = *it;
  remove(it, row, col);
  insert(point);
}



template<class Point>
const RasterSpace& GriddedPoints<Point>::space() const
{
  return d_space;
}



template<class Point>
const typename GriddedPoints<Point>::MVRaster&
         GriddedPoints<Point>::missingValues() const
{
  return d_missingValues;
}



template<class Point>
typename GriddedPoints<Point>::List& GriddedPoints<Point>::cell(
         LinearLoc loc)
{
  DEVELOP_PRECOND(!isMV(loc));

  return d_points.cell(loc);
}



template<class Point>
typename GriddedPoints<Point>::List const& GriddedPoints<Point>::cell(
         LinearLoc loc) const
{
  DEVELOP_PRECOND(!isMV(loc));

  return d_points.cell(loc);
}



template<class Point>
const typename GriddedPoints<Point>::List&
         GriddedPoints<Point>::cell(const CellLoc& loc) const
{
  DEVELOP_PRECOND(!isMV(loc));

  return d_points.cell(loc);
}



template<class Point>
typename GriddedPoints<Point>::List&
         GriddedPoints<Point>::cell(const CellLoc& loc)
{
  DEVELOP_PRECOND(!isMV(loc));

  return d_points.cell(loc);
}



template<class Point>
typename GriddedPoints<Point>::const_iterator GriddedPoints<Point>::begin(
                   size_t row, size_t col) const
{
  DEVELOP_PRECOND(!isMV(row, col));

  return d_points.cell(row, col).begin();
}



template<class Point>
typename GriddedPoints<Point>::const_iterator GriddedPoints<Point>::end(size_t row, size_t col) const
{
  DEVELOP_PRECOND(!isMV(row, col));

  return d_points.cell(row, col).end();
}



template<class Point>
typename GriddedPoints<Point>::iterator GriddedPoints<Point>::begin(size_t row, size_t col)
{
  DEVELOP_PRECOND(!isMV(row, col));

  return d_points.cell(row, col).begin();
}



template<class Point>
typename GriddedPoints<Point>::iterator GriddedPoints<Point>::end(size_t row, size_t col)
{
  DEVELOP_PRECOND(!isMV(row, col));

  return d_points.cell(row, col).end();
}



template<class Point>
typename GriddedPoints<Point>::iterator GriddedPoints<Point>::begin(const CellLoc& loc)
{
  DEVELOP_PRECOND(!isMV(loc.row(), loc.col()));

  return d_points.cell(loc.row(), loc.col()).begin();
}




template<class Point>
typename GriddedPoints<Point>::iterator GriddedPoints<Point>::end(const CellLoc& loc)
{
  DEVELOP_PRECOND(!isMV(loc.row(), loc.col()));

  return d_points.cell(loc.row(), loc.col()).end();
}



template<class Point>
typename GriddedPoints<Point>::const_iterator GriddedPoints<Point>::begin(const CellLoc& loc) const
{
  DEVELOP_PRECOND(!isMV(loc.row(), loc.col()));

  return d_points.cell(loc.row(), loc.col()).begin();
}




template<class Point>
typename GriddedPoints<Point>::const_iterator GriddedPoints<Point>::end(const CellLoc& loc) const
{
  DEVELOP_PRECOND(!isMV(loc.row(), loc.col()));

  return d_points.cell(loc.row(), loc.col()).end();
}



template<class Point>
bool GriddedPoints<Point>::empty() const
{
  return nrPoints() == 0;
}



template<class Point>
bool GriddedPoints<Point>::empty(LinearLoc loc) const
{
  DEVELOP_PRECOND(!isMV(loc));

  return d_points.cell(loc).empty();
}



template<class Point>
bool GriddedPoints<Point>::empty(size_t row, size_t col) const
{
  DEVELOP_PRECOND(!isMV(row, col));

  return d_points.cell(row, col).empty();
}



template<class Point>
size_t GriddedPoints<Point>::size() const
{
  size_t sum = 0;

  for(typename PointsRaster::const_iterator it = d_points.begin();
         it != d_points.end(); ++it) {
    sum += (*it).size();
  }

  return sum;
}



template<class Point>
size_t GriddedPoints<Point>::size(size_t row, size_t col) const
{
  DEVELOP_PRECOND(!isMV(row, col));

  return d_points.cell(row, col).size();
}



template<class Point>
size_t GriddedPoints<Point>::size(const CellLoc& loc) const
{
  return d_points.cell(loc).size();
}



template<class Point>
bool GriddedPoints<Point>::isMV(LinearLoc loc) const
{
  return d_missingValues.cell(loc);
}



template<class Point>
bool GriddedPoints<Point>::isMV(size_t row, size_t col) const
{
  return d_missingValues.cell(row, col);
}



template<class Point>
bool GriddedPoints<Point>::isMV(const CellLoc& loc) const
{
  return d_missingValues.cell(loc.row(), loc.col());
}



template<class Point>
size_t GriddedPoints<Point>::nrRows() const
{
  return d_space.nrRows();
}



template<class Point>
size_t GriddedPoints<Point>::nrCols() const
{
  return d_space.nrCols();
}



template<class Point>
size_t GriddedPoints<Point>::nrCells() const
{
  return d_space.nrCells();
}



template<class Point>
size_t GriddedPoints<Point>::nrPoints() const
{
  size_t sum = 0;

  for(size_t row = 0; row < nrRows(); ++row) {
    for(size_t col = 0; col < nrCols(); ++col) {
      sum += nrPoints(row, col);
    }
  }

  return sum;
}



template<class Point>
size_t GriddedPoints<Point>::nrPoints(size_t row, size_t col) const
{
  return !isMV(row, col) ? d_points.cell(row, col).size(): 0;
}



template<class Point>
size_t GriddedPoints<Point>::nrPoints(const CellLoc& loc) const
{
  return !isMV(loc) ? d_points.cell(loc).size(): 0;
}



/*
namespace geo {

template<class Point>
class GetPoints {

private:

  // All points.
  const GriddedPoints<Point>& d_griddedPoints;

  // Points within neighbourhood.
  std::vector<Point> d_points;

public:

  GetPoints(const GriddedPoints<Point>& griddedPoints)
    : d_griddedPoints(griddedPoints)
  {
  }

  bool operator()(size_t col, size_t row) {
    if(!d_griddedPoints.isMV(row, col)) {
      d_points.insert(d_points.end(), d_griddedPoints.begin(row, col),
         d_griddedPoints.end(row, col));
    }

    return true;
  }

  const std::vector<Point>& points() {
    return d_points;
  }
};

}
*/



template<class Point>
void GriddedPoints<Point>::points(const CellLoc& loc,
         double radius, std::vector<Point>& points) const
{
  CircularNeighbourhood neighbourhood(0, radius);
  this->points(loc, neighbourhood, points);
}



template<class Point>
void GriddedPoints<Point>::points(const CellLoc& loc,
         double radius, std::vector<Point*>& points)
{
  CircularNeighbourhood neighbourhood(0, radius);
  this->points(loc, neighbourhood, points);
}



template<class Point>
void GriddedPoints<Point>::points(const CellLoc& loc,
         const Neighbourhood& neighbourhood, std::vector<Point>& points) const
{
  int startRow, startCol, rowOffset, colOffset;

  if(loc.row() < neighbourhood.radius()) {
    startRow = 0;
    rowOffset = neighbourhood.radius() - loc.row();
  }
  else {
    startRow = loc.row() - neighbourhood.radius();
    rowOffset = -startRow;
  }

  if(loc.col() < neighbourhood.radius()) {
    startCol = 0;
    colOffset = neighbourhood.radius() - loc.col();
  }
  else {
    startCol = loc.col() - neighbourhood.radius();
    colOffset = -startCol;
  }

  size_t endRow = std::min(loc.row() + neighbourhood.radius(), nrRows() - 1);
  size_t endCol = std::min(loc.col() + neighbourhood.radius(), nrCols() - 1);

  for(size_t row = startRow; row <= endRow; ++row) {
    for(size_t col = startCol; col <= endCol; ++col) {
      if(neighbourhood.cell(rowOffset + row, colOffset + col) > 0.0 &&
         !isMV(row, col)) {
        points.insert(points.end(), begin(row, col), end(row, col));
      }
    }
  }
}



template<class Point>
void GriddedPoints<Point>::points(const CellLoc& loc,
         const Neighbourhood& neighbourhood, std::vector<Point*>& points)
{
  int startRow, startCol, rowOffset, colOffset;

  if(loc.row() < neighbourhood.radius()) {
    startRow = 0;
    rowOffset = neighbourhood.radius() - loc.row();
  }
  else {
    startRow = loc.row() - neighbourhood.radius();
    rowOffset = -startRow;
  }

  if(loc.col() < neighbourhood.radius()) {
    startCol = 0;
    colOffset = neighbourhood.radius() - loc.col();
  }
  else {
    startCol = loc.col() - neighbourhood.radius();
    colOffset = -startCol;
  }

  size_t endRow = std::min(loc.row() + neighbourhood.radius(), nrRows() - 1);
  size_t endCol = std::min(loc.col() + neighbourhood.radius(), nrCols() - 1);

  for(size_t row = startRow; row <= endRow; ++row) {
    for(size_t col = startCol; col <= endCol; ++col) {
      if(neighbourhood.cell(rowOffset + row, colOffset + col) > 0.0 &&
         !isMV(row, col)) {
        for(iterator it = begin(row, col); it != end(row, col); ++it) {
          points.push_back(&*it);
        }
      }
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace

