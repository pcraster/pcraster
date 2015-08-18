#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

/*!
 * default ctor, leaving object !valid(); nrRows and nrCols are 0,
 * other attributes have same defaults as other ctor, making these
 * two definitions identical:
 *
 * \code
 * RasterSpace rs;
 * assert(!rs.valid());
 * rs.setNrRows(3);
 * rs.setNrCols(3);
 * assert(rs.valid());
 *
 * RasterSpace rs(3,3); 
 * assert(rs.valid());
 * \endcode
 */
geo::RasterSpace::RasterSpace():
 RasterDim()
{
  d_cellSize   = 1;
  d_left       = 0;
  d_top        = 0;
  d_projection = YIncrB2T;
  d_angle      = 0;

  setup();
}

//! ctor, with nrRows and nrCols args and defaults
/*
 * \pre proj!=IllegalProjection
 */
geo::RasterSpace::RasterSpace(size_t nrRows, size_t nrCols,
 double cellSize,
 double leftWest, double topNorth, Projection proj, double angle ):
  RasterDim(nrRows,nrCols), d_cellSize(cellSize),
  d_left(leftWest), d_top(topNorth),
  d_angle(angle),
  d_projection(proj)
{
  PRECOND(proj!=IllegalProjection);
  setup();
}



geo::RasterSpace::RasterSpace(dal::Raster const& raster)
  : RasterDim(raster.nrRows(), raster.nrCols()), d_cellSize(raster.cellSize()),
    d_left(raster.west()), d_top(raster.north()),
    d_angle(0.0), d_projection(YIncrB2T)
{
  setup();
}



bool geo::RasterSpace::equals(const RasterSpace& c) const
{
  return (
    nrRows()     == c.nrRows() &&
    nrCols()     == c.nrCols() &&
    d_cellSize   == c.d_cellSize &&
    d_left       == c.d_left &&
    d_top        == c.d_top &&
    d_projection == c.d_projection &&
    d_angle == c.d_angle
    );
}

geo::RasterSpace::~RasterSpace()
{
}

void geo::RasterSpace::center(size_t r, size_t c, double &x, double &y) const
{
   rowCol2Coords(r+0.5, c+0.5, x, y);
}



void geo::RasterSpace::center(CellLoc const& loc, double &x, double &y) const
{
  center(loc.row(), loc.col(), x, y);
}



void geo::RasterSpace::upperLeft(size_t r, size_t c,
         double &x, double &y) const
{
   rowCol2Coords(r, c, x, y);
}



void geo::RasterSpace::lowerRight(size_t r, size_t c,
         double &x, double &y) const
{
   rowCol2Coords(r + 1, c + 1, x, y);
}


void geo::RasterSpace::setup(void)
{
/* set the map angle cosine and sin in header
 * these values are only used in the co-ordinate conversion
 * routines. And since they do a counter clockwise rotation we
 * take the sine and cosine of the negative angle.
 */
  d_angleCos = std::cos(-d_angle);
  d_angleSin = std::sin(-d_angle);
}

/*!
  \param     s New cellsize.
  \warning   \a s must be > 0.
*/
void geo::RasterSpace::setCellSize(double s)
{
#ifdef DEBUG_DEVELOP
  POSTCOND(s > 0);
#endif

  d_cellSize = s;
  setup();
}

/*!
  \param     x Left coordinate of raster.
  \param     y Top coordinate of raster.
*/
void geo::RasterSpace::setUpperLeft(double x, double y)
{
  d_left = x;
  d_top  = y;
  setup();
}


//! compute true world co-ordinate from row, column index
/*!rowCol2Coords computes the true world co-ordinate from a
 * row, column index. The row,column co-ordinate can be fractions.
 * For example (row,col) = (0.5,0.5) computes the (x,y) co-ordinate of
 * the centre of the upper left pixel. Secondly, the row and column co-ordinate
 * don't have to be on the map. They are just relative to upper left position.
 * For example (row,col) = (-0.5,0.5) computes the (x,y) co-ordinate of
 * the centre of the pixel that is right above upper left pixel.
 * <BR> Note code copy from csf/rcoords.c
 *
 * <ul>
 * <li>  \a row  row number (relates to y position).
 * <li>  \a col  column number (relates to x position).
 * <li>  \a x    write-only. x co-ordinate.
 * <li>  \a y    write-only. y co-ordinate.
 * </ul>
 */
void geo::RasterSpace::rowCol2Coords(
  double row,
  double col,
  double &x,
  double &y) const
{
  double yRow   = d_cellSize * row;
  double xCol   = d_cellSize * col;
  double xCol_t = xCol * d_angleCos - yRow * d_angleSin;
  double yRow_t = xCol * d_angleSin + yRow * d_angleCos;

  x = d_left + xCol_t;
  if(d_projection == YIncrT2B)
    y = d_top + yRow_t;
  else  /* all other projections */
    y = d_top - yRow_t;
}



//! Computes true world co-ordinate from cell location.
/*!
  \sa        rowCol2Coords(double, double, double&, double&)
*/
void geo::RasterSpace::loc2Coords(const CellLoc& loc, double& x,
         double& y) const
{
  rowCol2Coords(loc.row(), loc.col(), x, y);
}



//! Computes row and col indices from true world coordinates.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  First the fractional row and col indices are calculated which are then
  casted to an unsigned integer. This means the result of calculating the
  row and col indices must be larger or equal to 0.0.
*/
void geo::RasterSpace::coords2Loc(
         double x,
         double y,
         CellLoc& loc) const
{
  double row, col;
  coords2RowCol(x, y, row, col);
  loc.setIndices(static_cast<size_t>(row), static_cast<size_t>(col));
}



//! compute (fractional) row, column index from true world co-ordinate.
/*!
  \param     x x-coordinate.
  \param     y y-coordinate.
  \param     row Row number.
  \param     col Column number.

   coord2RowCol computes row, column index from true world co-ordinate.
   The row and column co-ordinate are returned as fractions.
   The x,y co-ordinate don't have to be on the map. They are just relative to
   upper left position.
*/
void geo::RasterSpace::coords2RowCol(double x, double y,
                   double& row, double& col) const
{
  double xCol = (x - d_left) / d_cellSize;
  double yRow;
  if(d_projection == YIncrT2B) {
    yRow = (y - d_top) / d_cellSize;
  }
  else {
    yRow = (d_top - y) / d_cellSize;
  }

  /* rotate clockwise: */
  double c = d_angleCos;     /* cos(t) == cos(-t) */
  double s = -(d_angleSin);  /* -sin(t) == sin(-t) */

  col = xCol * c - yRow * s;
  row = xCol * s + yRow * c;
}



//! Determines quadrant of cell where point \a x, \a y is located in.
/*!
  \param     x X coordinate of point.
  \param     y Y coordinate of point.
  \return    Quadrant.
*/
geo::Quadrant geo::RasterSpace::quadrant(double x, double y) const
{
  Quadrant quadrant(NorthWest);

  // Determine center of cell containing x, y.
  double row, col, xCenter, yCenter;
  coords2RowCol(x, y, row, col);
  rowCol2Coords(std::floor(row) + 0.5, std::floor(col) + 0.5, xCenter, yCenter);

  // Determine quadrant.
  double sign = projection() == geo::YIncrB2T ? -1.0 : 1.0;

  if(x < xCenter && (sign * (y - yCenter)) < 0.0) {
    // North west quadrant.
    quadrant = NorthWest;
  }
  else if(x >= xCenter && (sign * (y - yCenter)) < 0.0) {
    // North east quadrant.
    quadrant = NorthEast;
  }
  else if(x >= xCenter && (sign * (y - yCenter)) >= 0.0) {
    // South east quadrant.
    quadrant = SouthEast;
  }
  else if(x < xCenter && (sign * (y - yCenter)) >= 0.0){
    // South west quadrant.
    quadrant = SouthWest;
  }
  else {
    PRECOND(false);
  }

  return quadrant;
}



namespace geo {

//! Determines x and y world coordinates of \a cell.
/*!
  \param     x X-coordinate to set.
  \param     y Y-coordinate to set.
  \param     cell Location of cell to find coordinates of.
  \todo      Get rid of loc2Coords which doesn't adher to our styleguide.
*/
void RasterSpace::coordinates(
         double& x,
         double& y,
         CellLoc const& cell) const
{
  loc2Coords(cell, x, y);
}



//! Determines x and y world coordinates of \a cell.
/*!
  \param     x X-coordinate to set.
  \param     y Y-coordinate to set.
  \param     cell Location of cell to find coordinates of.
  \todo      Get rid of loc2Coords which doesn't adher to our styleguide.

  \todo      Get rid of rowCol2Coords which doesn't adher to our styleguide.
*/
void RasterSpace::coordinates(
         double& x,
         double& y,
         LinearLoc const& cell) const
{
  rowCol2Coords(cell / nrCols(), cell % nrCols(), x, y);
}

} // namespace



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Completely equal
/*! Note that is dangerous, since most fields are floats!
*/
bool geo::operator==(const RasterSpace& lhs, const RasterSpace& rhs)
{
  return lhs.equals(rhs);
}



bool geo::operator!=(const RasterSpace& lhs, const RasterSpace& rhs)
{
  return !lhs.equals(rhs);
}



std::ostream& geo::operator<<(std::ostream& s, const RasterSpace& rs)
{
  s << rs.nrRows() << ' ' << rs.nrCols() << ' ' << rs.d_cellSize << '\n'
    << rs.d_projection << ' ' << rs.d_left << ' ' << rs.d_top << '\n'
    << rs.d_angle << '\n';

  return s;
}



std::istream& geo::operator>>(std::istream& s, RasterSpace& rs)
{
  int p;
  size_t nrRows,nrCols;

  s >> nrRows >> nrCols >> rs.d_cellSize
                   >> p >> rs.d_left >> rs.d_top
                   >> rs.d_angle;

  rs.setNrRows(nrRows);
  rs.setNrCols(nrCols);

  rs.d_projection = static_cast<Projection>(p);

  switch(rs.d_projection) {
    case YIncrB2T:
    case YIncrT2B: break;
    default: throw com::BadStreamFormat("Rasterspace: Bad integer value for Projection");
  }
  if(!s.good())
    throw com::BadStreamFormat("Rasterspace: Bad format");

  rs.setup();

  return s;
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
