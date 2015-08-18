#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_NEIGHBOURHOOD
#include "geo_neighbourhood.h"
#define INCLUDED_GEO_NEIGHBOURHOOD
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_NUMERIC
#include <numeric>
#define INCLUDED_NUMERIC
#endif

// PCRaster library headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Neighbourhood class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class NeighbourhoodPrivate
{
public:

  NeighbourhoodPrivate()
  {
  }

  ~NeighbourhoodPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC NEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF NEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

geo::Neighbourhood::Neighbourhood(size_t radius)

  : geo::SimpleRaster<double>(2 * radius + 1, 2 * radius + 1, 0.0),
    d_radius(radius),
    d_fromRadius(0.0),
    d_toRadius(static_cast<double>(radius))

{
}



//! Constructor.
/*!
  \param     radius Radius of the neighbourhood.

  All cells in the neighbourhood are initialized with 0.0.

  The radius of the neighbourhood is taken to be \a toRadius. The radius of
  the inner shape is taken to be 0.
*/
geo::Neighbourhood::Neighbourhood(double toRadius)

  : geo::SimpleRaster<double>(
         2 * static_cast<size_t>(std::ceil(toRadius)) + 1,
         2 * static_cast<size_t>(std::ceil(toRadius)) + 1, 0.0),
    d_radius(static_cast<size_t>(std::ceil(toRadius))),
    d_fromRadius(0.0),
    d_toRadius(toRadius)

{
  PRECOND(toRadius >= 0.0);
}



geo::Neighbourhood::Neighbourhood(double fromRadius, double toRadius)

  : geo::SimpleRaster<double>(
         2 * static_cast<size_t>(std::ceil(toRadius)) + 1,
         2 * static_cast<size_t>(std::ceil(toRadius)) + 1, 0.0),
    d_radius(static_cast<size_t>(std::ceil(toRadius))),
    d_fromRadius(fromRadius),
    d_toRadius(toRadius)

{
  PRECOND(fromRadius >= 0.0);
  PRECOND(toRadius >= 0.0);
  PRECOND(d_fromRadius <= d_toRadius);
}



geo::Neighbourhood::Neighbourhood(size_t radius, double* cells)

  : geo::SimpleRaster<double>(2 * radius + 1, 2 * radius + 1, cells),
    d_radius(radius),
    d_fromRadius(0.0),
    d_toRadius(static_cast<double>(radius))

{
}



//! Destructor.
/*!
*/
geo::Neighbourhood::~Neighbourhood()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
geo::Neighbourhood& geo::Neighbourhood::operator=(const Neighbourhood& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
geo::Neighbourhood::Neighbourhood(const Neighbourhood& rhs):
  Base(rhs)
{
}
*/



//! Returns the radius of the neighbourhood.
/*!
  \return    Radius.
*/
size_t geo::Neighbourhood::radius() const
{
  return d_radius;
}



double geo::Neighbourhood::fromRadius() const
{
  return d_fromRadius;
}



double geo::Neighbourhood::toRadius() const
{
  return d_toRadius;
}



bool geo::Neighbourhood::hasDonutShape() const
{
  return d_fromRadius < d_toRadius;
}



bool geo::Neighbourhood::isOutline() const
{
  return d_fromRadius == d_toRadius;
}



double geo::Neighbourhood::sum() const
{
  return std::accumulate(begin(), end(), 0.0);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

namespace geo {

void cookieCut(
         size_t& minRow,
         size_t& maxRow,
         size_t& minCol,
         size_t& maxCol,
         RasterDim const& space,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell)
{
  minRow = std::max<int>(0, cell.row() - neighbourhood.radius());
  maxRow = std::min<int>(space.nrRows() - 1,
         cell.row() + neighbourhood.radius());
  minCol = std::max<int>(0, cell.col() - neighbourhood.radius());
  maxCol = std::min<int>(space.nrCols() - 1,
         cell.col() + neighbourhood.radius());

  DEVELOP_POSTCOND(minRow < space.nrRows());
  DEVELOP_POSTCOND(maxRow < space.nrRows());
  DEVELOP_POSTCOND(maxRow >= minRow);
  DEVELOP_POSTCOND(minCol < space.nrCols());
  DEVELOP_POSTCOND(maxCol < space.nrCols());
  DEVELOP_POSTCOND(maxCol >= minCol);
}



void selectSelectionCandidates(
         std::vector<LinearLoc>& cellIds,
         size_t minRow,
         size_t maxRow,
         size_t minCol,
         size_t maxCol,
         RasterDim const& space,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell)
{
  // Determine id's of cells which are candidates for selection.
  // -> cells with cell value != 0.0.
  for(size_t row = minRow; row <= maxRow; ++row) {
    for(size_t col = minCol; col <= maxCol; ++col) {
      // Row and col are space coordinates, not neighbourhood coordinates.
      // Substract upper left space coordinate of neighbourhood to get
      // neighbourhood coordinates.
      if(!dal::comparable(
         neighbourhood.cell(
              row - (cell.row() - neighbourhood.radius()),
              col - (cell.col() - neighbourhood.radius())),
              0.0)) {
        cellIds.push_back(space.convert(row, col));
      }
    }
  }
}



template<typename T>
void selectSelectionCandidates(
         std::vector<LinearLoc>& cellIds,
         size_t minRow,
         size_t maxRow,
         size_t minCol,
         size_t maxCol,
         SimpleRaster<T> const& raster,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell)
{
  // Determine id's of cells which are candidates for selection.
  // -> non mv cells with cell value != 0.0.
  for(size_t row = minRow; row <= maxRow; ++row) {
    for(size_t col = minCol; col <= maxCol; ++col) {
      // Row and col are space coordinates, not neighbourhood coordinates.
      // Substract upper left space coordinate of neighbourhood to get
      // neighbourhood coordinates.
      if(!raster.isMV(row, col) && !dal::comparable(
         neighbourhood.cell(
              row - (cell.row() - neighbourhood.radius()),
              col - (cell.col() - neighbourhood.radius())),
              0.0)) {
        cellIds.push_back(raster.convert(row, col));
      }
    }
  }
}



void selectRandomCellLocations(
         std::vector<LinearLoc>& locations,
         size_t nrCells,
         std::vector<LinearLoc>& cellIds)
{
  if(nrCells < cellIds.size()) {
    std::random_shuffle(cellIds.begin(), cellIds.end());
    locations.insert(locations.end(), cellIds.begin(),
         cellIds.begin() + nrCells);
  }
  else {
    locations.insert(locations.end(), cellIds.begin(), cellIds.end());
  }
}



//! Randomly selects \a nrCells cells from \a space which are within \a neighbourhood around \a location and adds them to \a locations.
/*!
  \param     locations Output vector for ids of selected cells. Ids will be
             appended to the end of the vector.
  \return    Selected cell ids appended to \a locations.

  If the neighbourhood around \a cell contains \a nrCells or less values, than
  all cells in the neighbourhood are returned.
*/
void randomCellLocations(
         std::vector<LinearLoc>& locations,
         size_t nrCells,
         RasterDim const& space,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell)
{
  DEVELOP_PRECOND(nrCells > 0);
  DEVELOP_PRECOND(space.nrCells() > 0);
  DEVELOP_PRECOND(neighbourhood.nrCells() > 0);
  DEVELOP_PRECOND(space.contains(cell));

  if(nrCells == 0 || space.nrCells() == 0 || neighbourhood.nrCells() == 0) {
    return;
  }

  size_t minRow, maxRow, minCol, maxCol;
  std::vector<LinearLoc> cellIds;

  cookieCut(minRow, maxRow, minCol, maxCol, space, neighbourhood, cell);
  selectSelectionCandidates(cellIds, minRow, maxRow, minCol, maxCol, space,
         neighbourhood, cell);
  selectRandomCellLocations(locations, nrCells, cellIds);
}



template<typename T>
void randomCellLocations(
         std::vector<LinearLoc>& locations,
         size_t nrCells,
         SimpleRaster<T> const& raster,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell)
{
  DEVELOP_PRECOND(nrCells > 0);
  DEVELOP_PRECOND(raster.nrCells() > 0);
  DEVELOP_PRECOND(neighbourhood.nrCells() > 0);
  DEVELOP_PRECOND(raster.contains(cell));

  if(nrCells == 0 || raster.nrCells() == 0 || neighbourhood.nrCells() == 0) {
    return;
  }

  size_t minRow, maxRow, minCol, maxCol;
  std::vector<LinearLoc> cellIds;

  cookieCut(minRow, maxRow, minCol, maxCol, raster, neighbourhood, cell);
  selectSelectionCandidates<T>(cellIds, minRow, maxRow, minCol, maxCol, raster,
         neighbourhood, cell);
  selectRandomCellLocations(locations, nrCells, cellIds);
}



template void randomCellLocations<UINT4>(
         std::vector<LinearLoc>& locations,
         size_t nrCells,
         SimpleRaster<UINT4> const& raster,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell);
template void randomCellLocations<REAL4>(
         std::vector<LinearLoc>& locations,
         size_t nrCells,
         SimpleRaster<REAL4> const& raster,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell);

template void selectSelectionCandidates<UINT4>(
         std::vector<LinearLoc>& cellIds,
         size_t minRow,
         size_t maxRow,
         size_t minCol,
         size_t maxCol,
         SimpleRaster<UINT4> const& raster,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell);
template void selectSelectionCandidates<REAL4>(
         std::vector<LinearLoc>& cellIds,
         size_t minRow,
         size_t maxRow,
         size_t minCol,
         size_t maxCol,
         SimpleRaster<REAL4> const& raster,
         Neighbourhood const& neighbourhood,
         CellLoc const& cell);

} // namespace
