#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PT_PARTICLETRACKER
#include "pt_ParticleTracker.h"
#define INCLUDED_PT_PARTICLETRACKER
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif

#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

#ifndef INCLUDED_GEO_ALGORITHM
#include "geo_algorithm.h"
#define INCLUDED_GEO_ALGORITHM
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#include "geo_griddedpoints.cc"

#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif

// Module headers.


/*
  TODO:

  optimizeConcentrationGradient
    implementeren
    vlak fitten door cel waardes.
*/



/*!
  \file
  This file contains the implementation of the ParticleTracker class.
*/



namespace {

template<typename InputIterator, typename OutputIterator>
void copy(
         InputIterator source,
         InputIterator sourceEnd,
         OutputIterator destination)
{
  for(; source != sourceEnd; ++source, ++destination) {
    if(pcr::isMV(*source)) {
      pcr::setMV(*destination);
    }
    else {
      *destination = *source;
    }
  }
}

} // Anonymous namespace



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARTICLETRACKER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PARTICLETRACKER MEMBERS
//------------------------------------------------------------------------------

pt::ParticleTracker::ParticleTracker(
         const geo::RasterSpace& space,
         double timeIncrement,
         UINT4 nrParticles,
         REAL4 const* iniConc,
         REAL4 const* effPorosity,
         REAL4 const* storageCoef)

  : _particles(space),
    _timeIncrement(timeIncrement),
    _nrParticles(nrParticles),
    _iniConc(nrRows(), nrCols()),
    _effPorosity(nrRows(), nrCols()),
    _storageCoef(nrRows(), nrCols()),
    _xBorderVeloc(nrRows(), nrCols()),
    _yBorderVeloc(nrRows(), nrCols()),
    _velocityMagnitude(nrRows(), nrCols()),
    _disp1(nrRows(), nrCols()),
    _disp2(nrRows(), nrCols()),
    _disp3(nrRows(), nrCols()),
    _disp4(nrRows(), nrCols())

{
  // Copy values from buffers into rasters, preserving mv's.
  copy(iniConc, iniConc + (nrRows() * nrCols()), _iniConc.cells());
  copy(effPorosity, effPorosity + (nrRows() * nrCols()), _effPorosity.cells());
  copy(storageCoef, storageCoef + (nrRows() * nrCols()), _storageCoef.cells());

  init();
}



//! Constructor.
/*!
  \param     space Properties of raster.
  \param     nrParticles Number of particles per cell.
  \warning   \a nrParticles must be 4, 5, 8 or 9.
  \sa        generateDistribution()
*/
pt::ParticleTracker::ParticleTracker(const geo::RasterSpace& space,
         double timeIncrement, UINT4 nrParticles,
         const geo::SimpleRaster<double>& iniConc,
         const geo::SimpleRaster<double>& effPorosity,
         const geo::SimpleRaster<double>& storageCoef)

  : _particles(space),
    _timeIncrement(timeIncrement),
    _nrParticles(nrParticles),
    _iniConc(iniConc),
    _effPorosity(effPorosity),
    _storageCoef(storageCoef),
    _xBorderVeloc(nrRows(), nrCols()),
    _yBorderVeloc(nrRows(), nrCols()),
    _velocityMagnitude(nrRows(), nrCols()),
    _disp1(nrRows(), nrCols()),
    _disp2(nrRows(), nrCols()),
    _disp3(nrRows(), nrCols()),
    _disp4(nrRows(), nrCols())

{
  init();
}


void pt::ParticleTracker::init()
{
  PRECOND(nrRows() > 0 && nrCols() > 0);
  PRECOND(_timeIncrement > 0.0);

  _gamma = 0.5;
  _initialMass = 0;

  if(!(_nrParticles == 4 || _nrParticles == 5 || _nrParticles == 8 ||
         _nrParticles == 9)) {
    throw com::Exception("Number of particles per cell must be 4, 5, 8 or 9");
  }

  determineAquifer(_iniConc);
  generateDistribution(_iniConc);

  checkWithAquifer(_effPorosity);
  checkWithAquifer(_storageCoef);
}



//! Destructor.
/*!
*/
pt::ParticleTracker::~ParticleTracker()
{
}



//! Determines the aquifer based on the defined values is \a raster.
/*!
  \param     raster Raster with valid values in aquifer cells.
  \exception .
  \warning   .
  \sa        .
*/
void pt::ParticleTracker::determineAquifer(
         const geo::SimpleRaster<double>& raster)
{
  for(geo::CellLocVisitor visitor(nrRows(), nrCols()); visitor.valid();
         ++visitor) {
    if(pcr::isMV(raster.cell(*visitor))) {
      _particles.setMV(*visitor);
    }
    else {
      PRECOND(!_particles.isMV(*visitor));
    }
  }
}



void pt::ParticleTracker::adjustAquifer(
         const geo::SimpleRaster<double>& raster)
{
  for(geo::CellLocVisitor visitor(nrRows(), nrCols()); visitor.valid();
         ++visitor) {
    if(inAquifer(*visitor) && pcr::isMV(raster.cell(*visitor))) {
      _particles.setMV(*visitor);
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Check warning and error handling with Cees.
*/
void pt::ParticleTracker::checkWithAquifer(
         const geo::SimpleRaster<double>& parameter)
{
  for(geo::CellLocVisitor visitor(nrRows(), nrCols()); visitor.valid();
         ++visitor) {
    /*
    Missing values are generated during calculation. We cannot distinguish
    between those and original missing values.
    if(!inAquifer(*visitor) && !pcr::isMV(parameter.cell(*visitor))) {
      // Parameter has value outside aquifer. This is bad practice.
      // TODO: libmisc: misc.h Warning functie gebruiken.
      std::cout << "Warning: Parameter has value outside of aquifer (row: "
                << visitor.row() << ", col: " << visitor.col() << ")" << std::endl;
      std::cout << "Warning: The extent of the aquifer was determined with" << std::endl;
      std::cout << "Warning: the initial concentration parameter" << std::endl;
    }
    */

    if(inAquifer(*visitor) && pcr::isMV(parameter.cell(*visitor))) {
      // Parameter has missing value within aquifer. This is an error.
      std::ostringstream stream;
      stream << "Parameter has missing value within the aquifer (row: "
             << visitor.row() << ", col: " << visitor.col() << ")";
      throw com::Exception(stream.str());
    }
  }
}



void pt::ParticleTracker::syncWithAquifer(geo::SimpleRaster<double>& raster)
{
  for(geo::CellLocVisitor visitor(nrRows(), nrCols()); visitor.valid();
         ++visitor) {
    if(!inAquifer(*visitor) && !pcr::isMV(raster.cell(*visitor))) {
      pcr::setMV(raster.cell(*visitor));
    }
  }
}



//! Returns whether the cell at \a loc lies in the aquifer.
/*!
  \param     loc Cell location.
  \return    true or false.
*/
bool pt::ParticleTracker::inAquifer(const geo::CellLoc& loc) const
{
  return !_particles.isMV(loc);
}



bool pt::ParticleTracker::inAquifer(size_t row, size_t col) const
{
  return !_particles.isMV(row, col);
}



bool pt::ParticleTracker::inAquifer(double x, double y) const
{
  double row, col;

  _particles.space().coords2RowCol(x, y, row, col);
  row = std::floor(row);
  col = std::floor(col);

  return row >= 0.0 && row < nrRows() && col >= 0.0 && col < nrCols() &&
              inAquifer(static_cast<size_t>(row), static_cast<size_t>(col));
}



bool pt::ParticleTracker::isSourceOrSinkCell(
         const geo::SimpleRaster<double>& flux, size_t row, size_t col) const
{
  return flux.cell(row, col) != 0.0;
}



bool pt::ParticleTracker::isSourceOrSinkCell(
         const geo::SimpleRaster<double>& flux, const geo::CellLoc& loc) const
{
  return flux.cell(loc.row(), loc.col()) != 0.0;
}



bool pt::ParticleTracker::isSourceCell(const geo::SimpleRaster<double>& flux,
         size_t row, size_t col) const
{
  return flux.cell(row, col) > 0.0;
}



bool pt::ParticleTracker::isSourceCell(const geo::SimpleRaster<double>& flux,
         const geo::CellLoc& loc) const
{
  return flux.cell(loc.row(), loc.col()) > 0.0;
}



//! Returns true if cell \a row, \a col is a sink cell.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Add constant head-sink case.

  A sink cell is a pumping sink (negative flux) or a constant head-sink.
*/
bool pt::ParticleTracker::isSinkCell(const geo::SimpleRaster<double>& flux,
         size_t row, size_t col) const
{
  return flux.cell(row, col) < 0.0;
}



bool pt::ParticleTracker::isSinkCell(const geo::SimpleRaster<double>& flux,
         const geo::CellLoc& loc) const
{
  return flux.cell(loc.row(), loc.col()) < 0.0;
}



bool pt::ParticleTracker::cellAlongEdgeAquifer(size_t row, size_t col) const
{
  PRECOND(row < nrRows() && col < nrCols());

  return row == 0 || row == nrRows() - 1 || col == 0 || col == nrCols() - 1 ||
         !inAquifer(row - 1, col) || !inAquifer(row + 1, col) ||
         !inAquifer(row, col - 1) || !inAquifer(row, col + 1);
}



geo::RasterSpace const& pt::ParticleTracker::space() const
{
  return _particles.space();
}



size_t pt::ParticleTracker::nrRows() const
{
  return _particles.space().nrRows();
}



size_t pt::ParticleTracker::nrCols() const
{
  return _particles.space().nrCols();
}



double pt::ParticleTracker::timeIncrement() const
{
  return _timeIncrement;
}



double pt::ParticleTracker::cellSize() const
{
  return _particles.space().cellSize();
}



double pt::ParticleTracker::cellWidth() const
{
  return cellSize();
}



double pt::ParticleTracker::cellHeight() const
{
  return cellSize();
}



double pt::ParticleTracker::cellArea() const
{
  return com::pow2(cellSize());
}



void pt::ParticleTracker::coords2RowCol(double x, double y, double& row,
         double& col) const
{
  _particles.space().coords2RowCol(x, y, row, col);
}



void pt::ParticleTracker::coords2RowCol(double x, double y, size_t& row,
         size_t& col) const
{
  double rowTmp, colTmp;
  coords2RowCol(x, y, rowTmp, colTmp);
  POSTCOND(rowTmp >= 0.0 && colTmp >= 0.0);
  row = static_cast<size_t>(std::floor(rowTmp));
  col = static_cast<size_t>(std::floor(colTmp));
}



void pt::ParticleTracker::rowCol2Coords(double row, double col, double& x,
         double& y) const
{
  _particles.space().rowCol2Coords(row, col, x, y);
}



void pt::ParticleTracker::loc2Coords(const geo::CellLoc& loc, double& x,
         double& y) const
{
  _particles.space().loc2Coords(loc, x, y);
}



//! Reflects point \a x, \a y.
/*!
  \param     row Row of cell where point \a x, \a y originated.
  \param     col Col of cell where point \a x, \a y originated.
  \param     x X coordinate of new location of point.
  \param     y Y coordinate of new location of point.
  \return    Updated values for \a x and \a y.
*/
void pt::ParticleTracker::reflect(size_t row, size_t col, double& x,
         double& y) const
{
  double rowNew, colNew;
  coords2RowCol(x, y, rowNew, colNew);

  // Distance from border current cell and new cell.
  double xDistance = 1.0 - (rowNew - std::floor(rowNew));
  double yDistance = 1.0 - (colNew - std::floor(colNew));
  POSTCOND(xDistance > 0.0 && xDistance <= 1.0);
  POSTCOND(yDistance > 0.0 && yDistance <= 1.0);
  rowNew = std::floor(rowNew);
  colNew = std::floor(colNew);
  POSTCOND(rowNew != row || colNew != col);

  if(rowNew < row) {
    if(colNew < col) {
      // Upper left.
      if(xDistance >= yDistance) {
        // Reflect in row.
        rowCol2Coords(row + yDistance, colNew, x, y);
      }
      else {
        // Reflect in column.
        rowCol2Coords(rowNew, col + xDistance, x, y);
      }
    }
    else if(colNew > col) {
      // Upper right.
      if(xDistance >= yDistance) {
        // Reflect in row.
        rowCol2Coords(row + yDistance, colNew, x, y);
      }
      else {
        // Reflect in col.
        rowCol2Coords(rowNew, colNew - xDistance, x, y);
      }
    }
    else {
      // Top.
      PRECOND(col == colNew);
      rowCol2Coords(row + yDistance, colNew, x, y);
    }
  }
  else if(rowNew > row) {
    if(colNew < col) {
      // Lower left.
      if(xDistance >= yDistance) {
        // Reflect in row.
        rowCol2Coords(rowNew - yDistance, colNew, x, y);
      }
      else {
        // Reflect in col.
        rowCol2Coords(rowNew, col + xDistance, x, y);
      }
    }
    else if(colNew > col) {
      // Lower right.
      if(xDistance >= yDistance) {
        // Reflect in Row.
        rowCol2Coords(rowNew - yDistance, colNew, x, y);
      }
      else {
        // Reflect in col.
        rowCol2Coords(rowNew, colNew - xDistance, x, y);
      }
    }
    else {
      // Bottom.
      PRECOND(col == colNew);
      rowCol2Coords(rowNew - yDistance, colNew, x, y);
    }
  }

  else if(colNew > col) {
    // Right.
    PRECOND(row == rowNew);
    rowCol2Coords(rowNew, colNew - xDistance, x, y);
  }
  else if(colNew < col) {
    // Left.
    PRECOND(row == rowNew);
    rowCol2Coords(rowNew, col + xDistance, x, y);
  }
  else {
    PRECOND(false);
  }
}



//! Generates a uniform distribution of tracer particles.
/*!
  \warning   All existing particles will be deleted.
  \sa        updateDistribution()

  This function assumes that at all location where _particles is defined
  \a raster also has a valid value.
*/
void pt::ParticleTracker::generateDistribution(
         const geo::SimpleRaster<double>& conc)
{
  // Remove all existing particles.
  _particles.clear();

  // Calculate half and third cell sizes (d2 and d3).
  double d2 = cellSize() / 2;
  double d3 = cellSize() / 3;
  double sign = _particles.space().projection() == geo::YIncrB2T ? -1.0 : 1.0;

  if(_nrParticles == 4 || _nrParticles == 5 || _nrParticles == 8 ||
         _nrParticles == 9) {

    // +-----------+
    // | p1     p2 |
    // |           |
    // |           |
    // |           |
    // | p4     p3 |
    // +-----------+

    Particle p1, p2, p3, p4;
    double left, right, top, bottom;

    for(size_t row = 0; row < nrRows(); ++row) {
      for(size_t col = 0; col < nrCols(); ++col) {

        if(!_particles.isMV(row, col)) {
          PRECOND(!pcr::isMV(conc.cell(row, col)));

          _particles.space().upperLeft(row, col, left, top);
          _particles.space().lowerRight(row, col, right, bottom);

          // Calculate coordinates of p1.
          p1[0] = left + d3;
          p1[1] = top + sign * d3;
          p1.setBirthCell(row, col);
          p1.setConcentration(conc.cell(row, col));

          // Calculate coordinates of p2.
          p2[0] = right - d3;
          p2[1] = top + sign * d3;
          p2.setBirthCell(row, col);
          p2.setConcentration(conc.cell(row, col));

          // Calculate coordinates of p3.
          p3[0] = right - d3;
          p3[1] = bottom - sign * d3;
          p3.setBirthCell(row, col);
          p3.setConcentration(conc.cell(row, col));

          // Calculate coordinates of p4.
          p4[0] = left + d3;
          p4[1] = bottom - sign * d3;
          p4.setBirthCell(row, col);
          p4.setConcentration(conc.cell(row, col));

          _particles.insert(p1);
          _particles.insert(p2);
          _particles.insert(p3);
          _particles.insert(p4);
        }
      }
    }
  }

  if(_nrParticles == 5 || _nrParticles == 9) {

    // +-----------+
    // |           |
    // |           |
    // |     p     |
    // |           |
    // |           |
    // +-----------+

    Particle p;

    for(size_t row = 0; row < nrRows(); ++row) {
      for(size_t col = 0; col < nrCols(); ++col) {

        if(!_particles.isMV(row, col)) {
          PRECOND(!pcr::isMV(conc.cell(row, col)));

          // Calculate coordinates of center of cell.
          _particles.space().center(row, col, p[0], p[1]);
          p.setBirthCell(row, col);
          p.setConcentration(conc.cell(row, col));
          _particles.insert(p);
        }
      }
    }
  }

  if(_nrParticles == 8 || _nrParticles == 9) {

    // +-----------+
    // |    p1     |
    // |           |
    // | p4     p2 |
    // |           |
    // |    p3     |
    // +-----------+

    Particle p1, p2, p3, p4;
    double left, right, top, bottom;

    for(size_t row = 0; row < nrRows(); ++row) {
      for(size_t col = 0; col < nrCols(); ++col) {

        if(!_particles.isMV(row, col)) {
          PRECOND(!pcr::isMV(conc.cell(row, col)));

          _particles.space().upperLeft(row, col, left, top);
          _particles.space().lowerRight(row, col, right, bottom);

          // Calculate coordinates of p1.
          p1[0] = left + d2;
          p1[1] = top + sign * d3;
          p1.setBirthCell(row, col);
          p1.setConcentration(conc.cell(row, col));

          // Calculate coordinates of p2.
          p2[0] = right - d3;
          p2[1] = top + sign * d2;
          p2.setBirthCell(row, col);
          p2.setConcentration(conc.cell(row, col));

          // Calculate coordinates of p3.
          p3[0] = left + d2;
          p3[1] = bottom - sign * d3;
          p3.setBirthCell(row, col);
          p3.setConcentration(conc.cell(row, col));

          // Calculate coordinates of p4.
          p4[0] = left + d3;
          p4[1] = top + sign * d2;
          p4.setBirthCell(row, col);
          p4.setConcentration(conc.cell(row, col));

          _particles.insert(p1);
          _particles.insert(p2);
          _particles.insert(p3);
          _particles.insert(p4);
        }
      }
    }
  }
}



//! Checks the current distribution of particles and improves when necessary.
/*!
  \sa        generateDistribution()

  Improves distribution when too many cells have not enough or too many
  particles. This function takes the original concentration gradient in
  each cell into account.
*/
void pt::ParticleTracker::updateDistribution(
         const geo::SimpleRaster<double>& flux)
{
  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {

    // Is location in a pumping or constant-head sink?
    if(!_particles.isMV(*loc) && isSinkCell(flux, *loc)) {

      geo::GriddedPoints<Particle>::List& particles = _particles.cell(*loc);
      geo::GriddedPoints<Particle>::iterator it = particles.begin();

      while(it != particles.end()) {

        if((*it).born(*loc)) {
          ++it;
        }
        else {
          // Remove particle from grid.
          it = particles.erase(it);
        }
      }
    }
  }

  #ifdef DEBUG_DEVELOP
  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc) && isSinkCell(flux, *loc)) {
      for(geo::GriddedPoints<Particle>::iterator it =
         _particles.cell(*loc).begin(); it != _particles.cell(*loc).end();
         ++it) {
        PRECOND((*it).born(*loc));
      }
    }
  }
  #endif

  // Special problem 6.
  if(percentageVoidCells() >= 1.0) {
    geo::SimpleRaster<double> conc(nrRows(), nrCols());
    averageConcentration(conc);
    generateDistribution(conc);

    // Special problem 7.
    optimizeConcentrationGradient();
  }
}



void pt::ParticleTracker::optimizeConcentrationGradient()
{
  // TODO
}



//! Calculates the velocity on the four cell borders.
/*!
  \param     xVeloc Velocity in x direction at cell center.
  \param     yVeloc Velocity in y direction at cell center.
  \exception .
  \warning   .
  \sa        .

  We have stream velocities in x and y direction of the cell centers, but we
  need them also on the center of the 4 cell borders.
*/
void pt::ParticleTracker::calculateVelocity(
         const geo::SimpleRaster<double>& xVeloc,
         const geo::SimpleRaster<double>& yVeloc)
{
  // Convert raster cell values to raster boundary values.
  geo::raster2Boundaries(xVeloc, _xBorderVeloc);
  geo::raster2Boundaries(yVeloc, _yBorderVeloc);

  // Calculate magnitude of velocity vector at cell boundaries.
  geo::magnitude(_xBorderVeloc, _yBorderVeloc, _velocityMagnitude);
}



//! Calculates dispersion equation coefficients on cell boundaries.
/*!
  \param     longDisp Longitudinal dispersivity.
  \param     transDisp Transverse dispersivity.
  \param     satThickness Saturated thickness of the aquifer.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void pt::ParticleTracker::calculateDispersionEquationCoefficients(
         const geo::SimpleRaster<double>& longDisp,
         const geo::SimpleRaster<double>& transDisp,
         const geo::SimpleRaster<double>& satThickness,
         geo::RasterBoundaries<double>& Dxx,
         geo::RasterBoundaries<double>& Dyy)
{
  // Rewrite equations for coefficients gives:
  // Dxx = (alphaL * Vx^2 / |V|) + (alphaT * Vy^2 / |V|)
  // Dyy = (alphaT * Vx^2 / |V|) + (alphaL * Vy^2 / |V|)
  // Dxy = Dyx = (alphaL * Vx * Vy - alphaT * Vx * Vy) / |V|

  geo::RasterBoundaries<double> Dxy(nrRows(), nrCols());
  geo::RasterBoundaries<double> b(nrRows(), nrCols());

  {
    // Temp vars.
    geo::RasterBoundaries<double> alphaL(nrRows(), nrCols());
    geo::RasterBoundaries<double> alphaT(nrRows(), nrCols());

    // Convert raster cell values to raster boundary values.
    geo::raster2Boundaries(longDisp, alphaL);
    geo::raster2Boundaries(transDisp, alphaT);
    geo::raster2Boundaries(satThickness, b);

    geo::RasterBoundaries<double> Vxx(_xBorderVeloc);
    Vxx *= _xBorderVeloc;
    geo::RasterBoundaries<double> Vyy(_yBorderVeloc);
    Vyy *= _yBorderVeloc;
    geo::RasterBoundaries<double> Vxy(_xBorderVeloc);
    Vxy *= _yBorderVeloc;

    Dxx = (alphaL * Vxx + alphaT * Vyy) / _velocityMagnitude;
    Dyy = (alphaT * Vxx + alphaL * Vyy) / _velocityMagnitude;
    Dxy = (alphaL * Vxy - alphaT * Vxy) / _velocityMagnitude;
  }

  // ---------------------------------------------------------------------------
  geo::SimpleRaster<double> b_right(nrRows(), nrCols());
  geo::SimpleRaster<double> b_bottom(nrRows(), nrCols());
  geo::SimpleRaster<double> Dxx_right(nrRows(), nrCols());
  geo::SimpleRaster<double> Dyy_bottom(nrRows(), nrCols());
  geo::SimpleRaster<double> Dxy_right(nrRows(), nrCols());
  geo::SimpleRaster<double> Dxy_bottom(nrRows(), nrCols());

  b.rightBoundary(b_right);
  b.bottomBoundary(b_bottom);
  Dxx.rightBoundary(Dxx_right);
  Dyy.bottomBoundary(Dyy_bottom);
  Dxy.rightBoundary(Dxy_right);
  Dxy.bottomBoundary(Dxy_bottom);

  // i + 1/2, j -> right side
  _disp1 = b_right * Dxx_right / com::pow2(cellWidth());

  // i, j + 1/2 -> bottom side
  _disp2 = b_bottom * Dyy_bottom / com::pow2(cellHeight());

  // i + 1/2, j -> right side
  _disp3 = b_right * Dxy_right /
         static_cast<double>(4 * cellWidth() * cellHeight());

  // i, j + 1/2 -> bottom side
  _disp4 = b_bottom * Dxy_bottom /
         static_cast<double>(4 * cellWidth() * cellHeight());

  // These variables might introduce missing values within the aquifer.
  adjustAquifer(_disp1);
  adjustAquifer(_disp2);
  adjustAquifer(_disp3);
  adjustAquifer(_disp3);

  // Calculation of these variables might have generated non-MV's outside the
  // aquifer.
  syncWithAquifer(_disp1);
  syncWithAquifer(_disp2);
  syncWithAquifer(_disp3);
  syncWithAquifer(_disp4);

  // TODO: not needed?
  checkWithAquifer(_disp1);
  checkWithAquifer(_disp2);
  checkWithAquifer(_disp3);
  checkWithAquifer(_disp4);
}



size_t pt::ParticleTracker::calculateNumberOfParticleMoves(
         const geo::SimpleRaster<double>& xVeloc,
         const geo::SimpleRaster<double>& yVeloc,
         const geo::SimpleRaster<double>& flux,
         const geo::SimpleRaster<double>& satThickness,
         const geo::RasterBoundaries<double>& Dxx,
         const geo::RasterBoundaries<double>& Dyy)
{
  // TODO: calc on cell nodes, not on boundaries!

  const size_t nrCriteria = 4;
  double criteria[nrCriteria];
  double tmp;

  pcr::setMV(&(criteria[0]), nrCriteria);

  // First criterium:
  // The smallest delta t will occur at the node having the largest value
  // of Dxx / (delta x)^2 + Dyy / (delta y)^2.
  tmp = (Dxx / com::pow2(cellWidth()) +
         Dyy / com::pow2(cellHeight())).maximum();
  POSTCOND(!pcr::isMV(tmp));
  criteria[0] = ABS(0.5 / tmp);

  // Second criterium:
  if(com::minimum(flux.begin(), flux.end()) != 0.0) {
    geo::SimpleRaster<double> tmp(
         (_effPorosity * satThickness / flux).absolute());
    criteria[1] = com::minimum(tmp.begin(), tmp.end());
    POSTCOND(!pcr::isMV(criteria[1]));
  }

  // Third and fourth criteria:
  geo::SimpleRaster<double> tmpRaster(xVeloc);
  tmpRaster.absolute();
  tmp = com::maximum(tmpRaster.begin(), tmpRaster.end());
  // tmp = geo::SimpleRaster<double>(xVeloc).absolute().maximum();
  POSTCOND(!pcr::isMV(tmp));
  POSTCOND(tmp >= 0.0);
  criteria[2] = _gamma * cellWidth() / tmp;

  tmpRaster = yVeloc;
  tmpRaster.absolute();
  tmp = com::maximum(tmpRaster.begin(), tmpRaster.end());
  // tmp = geo::SimpleRaster<double>(yVeloc).absolute().maximum();
  POSTCOND(!pcr::isMV(tmp));
  POSTCOND(tmp >= 0.0);
  criteria[3] = _gamma * cellHeight() / tmp;

  PRECOND(com::minimum(criteria, criteria + nrCriteria) > 0.0);
  size_t nrMoves = 1;
  if(_timeIncrement > com::minimum(criteria, criteria + nrCriteria)) {
    nrMoves = static_cast<size_t>(std::ceil(
         _timeIncrement / com::minimum(criteria, criteria + nrCriteria)));
  }
  POSTCOND(nrMoves > 0);

  return nrMoves;
}



double pt::ParticleTracker::xVelocity(const geo::SimpleRaster<double>& xVeloc,
         const geo::SimpleRaster<double>& flux,
         size_t row, size_t col, geo::Quadrant quadrant) const
{
  double velocities[4];
  pcr::setMV(&(velocities[0]), 4);

  PRECOND(!pcr::isMV(xVeloc.cell(row, col)));
  PRECOND(!pcr::isMV(_xBorderVeloc.topBoundary(row, col)));
  PRECOND(!pcr::isMV(_xBorderVeloc.rightBoundary(row, col)));
  PRECOND(!pcr::isMV(_xBorderVeloc.bottomBoundary(row, col)));
  PRECOND(!pcr::isMV(_xBorderVeloc.leftBoundary(row, col)));

  // i: column number
  // j: row number

  if(quadrant == geo::NorthWest) {
    // i      , j
    // i - 1/2, j
    // i - 1/2, j - 1
    // i      , j - 1
    // Special problem 2.
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = xVeloc.cell(row, col);
    }
    else {
      velocities[0] = _xBorderVeloc.leftBoundary(row, col);
    }
    velocities[1] = _xBorderVeloc.leftBoundary(row, col);
    if(row > 0) {
      velocities[2] = _xBorderVeloc.leftBoundary(row - 1, col);
      // Special problem 2.
      if(!isSourceOrSinkCell(flux, row - 1, col)) {
        velocities[3] = xVeloc.cell(row - 1, col);
      }
      else {
        velocities[3] = _xBorderVeloc.leftBoundary(row - 1, col);
      }
    }
  }
  else if(quadrant == geo::NorthEast) {
    // i      , j
    // i      , j - 1
    // i + 1/2, j - 1
    // i + 1/2, j
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = xVeloc.cell(row, col);
    }
    else {
      velocities[0] = _xBorderVeloc.rightBoundary(row, col);
    }
    if(row > 0) {
      if(!isSourceOrSinkCell(flux, row - 1, col)) {
        velocities[1] = xVeloc.cell(row - 1, col);
      }
      else {
        velocities[1] = _xBorderVeloc.rightBoundary(row - 1, col);
      }
      velocities[2] = _xBorderVeloc.rightBoundary(row - 1, col);
    }
    velocities[3] = _xBorderVeloc.rightBoundary(row, col);
  }
  else if(quadrant == geo::SouthEast) {
    // i      , j
    // i + 1/2, j
    // i + 1/2, j + 1
    // i      , j + 1
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = xVeloc.cell(row, col);
    }
    else {
      velocities[0] = _xBorderVeloc.rightBoundary(row, col);
    }
    velocities[1] = _xBorderVeloc.rightBoundary(row, col);
    if(row < nrRows() - 1) {
      velocities[2] = _xBorderVeloc.rightBoundary(row + 1, col);
      if(!isSourceOrSinkCell(flux, row + 1, col)) {
        velocities[3] = xVeloc.cell(row + 1, col);
      }
      else {
        velocities[3] = _xBorderVeloc.rightBoundary(row + 1, col);
      }
    }
  }
  else if(quadrant == geo::SouthWest) {
    // i      , j
    // i      , j + 1
    // i - 1/2, j + 1
    // i - 1/2, j
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = xVeloc.cell(row, col);
    }
    else {
      velocities[0] = _xBorderVeloc.leftBoundary(row, col);
    }
    if(row < nrRows() - 1) {
      if(!isSourceOrSinkCell(flux, row + 1, col)) {
        velocities[1] = xVeloc.cell(row + 1, col);
      }
      else {
        velocities[1] = _xBorderVeloc.leftBoundary(row + 1, col);
      }
      velocities[2] = _xBorderVeloc.leftBoundary(row + 1, col);
    }
    velocities[3] = _xBorderVeloc.leftBoundary(row, col);
  }
  else {
    PRECOND(false);
  }

  return com::average(velocities, velocities + 4);
}



double pt::ParticleTracker::yVelocity(const geo::SimpleRaster<double>& yVeloc,
         const geo::SimpleRaster<double>& flux,
         size_t row, size_t col, geo::Quadrant quadrant) const
{
  double velocities[4];
  pcr::setMV(&(velocities[0]), 4);

  if(quadrant == geo::NorthWest) {
    // i    , j
    // i - 1, j
    // i - 1, j - 1/2
    // i    , j - 1/2
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = yVeloc.cell(row, col);
    }
    else {
      velocities[0] = _yBorderVeloc.topBoundary(row, col);
    }
    if(col > 0) {
      if(!isSourceOrSinkCell(flux, row, col - 1)) {
        velocities[1] = yVeloc.cell(row, col - 1);
      }
      else {
        velocities[1] = _yBorderVeloc.topBoundary(row, col - 1);
      }
      velocities[2] = _yBorderVeloc.topBoundary(row, col - 1);
    }
    velocities[3] = _yBorderVeloc.topBoundary(row, col);
  }
  else if(quadrant == geo::NorthEast) {
    // i    , j
    // i    , j - 1/2
    // i + 1, j - 1/2
    // i + 1, j
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = yVeloc.cell(row, col);
    }
    else {
      velocities[0] = _yBorderVeloc.topBoundary(row, col);
    }
    velocities[1] = _yBorderVeloc.topBoundary(row, col);
    if(col < nrCols() - 1) {
      velocities[2] = _yBorderVeloc.topBoundary(row, col + 1);
      if(!isSourceOrSinkCell(flux, row, col + 1)) {
        velocities[3] = yVeloc.cell(row, col + 1);
      }
      else {
        velocities[3] = _yBorderVeloc.topBoundary(row, col + 1);
      }
    }
  }
  else if(quadrant == geo::SouthEast) {
    // i    , j
    // i + 1, j
    // i + 1, j + 1/2
    // i    , j + 1/2
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = yVeloc.cell(row, col);
    }
    else {
      velocities[0] = _yBorderVeloc.bottomBoundary(row, col);
    }
    if(col < nrCols() - 1) {
      if(!isSourceOrSinkCell(flux, row, col + 1)) {
        velocities[1] = yVeloc.cell(row, col + 1);
      }
      else {
        velocities[1] = _yBorderVeloc.bottomBoundary(row, col + 1);
      }
      velocities[2] = _yBorderVeloc.bottomBoundary(row, col + 1);
    }
    velocities[3] = _yBorderVeloc.bottomBoundary(row, col);
  }
  else if(quadrant == geo::SouthWest) {
    // i    , j
    // i    , j + 1/2
    // i - 1, j + 1/2
    // i - 1, j
    if(!isSourceOrSinkCell(flux, row, col)) {
      velocities[0] = yVeloc.cell(row, col);
    }
    else {
      velocities[0] = _yBorderVeloc.bottomBoundary(row, col);
    }
    velocities[1] = _yBorderVeloc.bottomBoundary(row, col);
    if(col > 0) {
      velocities[2] = _yBorderVeloc.bottomBoundary(row, col - 1);
      if(!isSourceOrSinkCell(flux, row, col - 1)) {
        velocities[3] = yVeloc.cell(row, col - 1);
      }
      else {
        velocities[3] = _yBorderVeloc.bottomBoundary(row, col - 1);
      }
    }
  }
  else {
    PRECOND(false);
  }

  return com::average(velocities, velocities + 4);
}



void pt::ParticleTracker::velocity(
         const geo::SimpleRaster<double>& xVeloc,
         const geo::SimpleRaster<double>& yVeloc,
         const geo::SimpleRaster<double>& flux,
         size_t row, size_t col, geo::Quadrant quadrant,
         double& xVelocParticle, double& yVelocParticle) const
{
  xVelocParticle = xVelocity(xVeloc, flux, row, col, quadrant);
  yVelocParticle = yVelocity(yVeloc, flux, row, col, quadrant);
}



/*
double pt::ParticleTracker::averageConcentration(const geo::CellLoc& loc) const
{
  double sum, average;

  if(_particles.isMV(loc)) {
    pcr::setMV(average);
  }
  else {
    if(_particles.nrPoints(loc) == 0) {
      average = 0.0;
    }
    else {
      sum = 0.0;
      for(geo::GriddedPoints<Particle>::const_iterator it =
         _particles.begin(loc); it != _particles.end(loc); ++it) {
        PRECOND((*it).concentration() >= 0.0);
        sum += (*it).concentration();
      }
      average = sum / _particles.nrPoints(loc);
    }

    POSTCOND(average >= 0.0);
  }

  return average;
}
*/



//! Calculates a value for cell \a loc based on surrounding values.
/*!
  \param     loc Cell location.
  \return    Concentration value.
*/
double pt::ParticleTracker::concentration(
         const std::vector<geo::PointValue<geo::Point<double, 2>, double > >& points,
         const geo::CellLoc& loc) const
{
  // interpolateConcentration(points, *loc);
  return maxConcentration(points, loc);
}



//! Calculates a value for cell \a loc based on surrounding values.
/*!
  \param     loc Cell location.
  \return    Concentration value.
*/
/*
double pt::ParticleTracker::interpolateConcentration(
         const std::vector<geo::IdiPoint<geo::Point<double, 2> > >& points,
         const geo::CellLoc& loc) const
{
  DEVELOP_PRECOND(!points.empty());

  double value;
  double power = 2;
  size_t maxNr = 8;
  double maxRadius = 2.0 * cellSize();

  double x, y;
  loc2Coords(loc, x, y);
  geo::Point<double, 2> point(x, y);
  // TODO: make sure that coords of points and radius are in world coords.
  if(!geo::idi(value, points, power, maxNr, maxRadius, point)) {
    Warning("interpolation of concentrations failed");
    // pcr::setMV(value);
    value = 0.0;
  }

  return value;
}
*/



double pt::ParticleTracker::maxConcentration(
         const std::vector<geo::PointValue<geo::Point<double, 2>, double> >& points,
         const geo::CellLoc& loc) const
{
  DEVELOP_PRECOND(!points.empty());

  double value;
  double maxRadius = 2.0 * cellSize();

  double x, y;
  loc2Coords(loc, x, y);
  geo::Point<double, 2> point(x, y);

  if(!geo::maximum(value, point, maxRadius, points.begin(), points.end())) {
    Warning("failed to determine maximum concentration");
    // pcr::setMV(value);
    value = 0.0;
  }

  DEVELOP_PRECOND(value >= 0.0);

  return value;
}



void pt::ParticleTracker::averageConcentration(
         geo::SimpleRaster<double>& conc) const
{
  // Select all points.
  /*
  typedef geo::Point<double, 2> Point;
  std::vector<geo::IdiPoint<Point> > points;
  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc)) {
      for(geo::GriddedPoints<Particle>::const_iterator it =
         _particles.begin(*loc); it != _particles.end(*loc); ++it) {
        points.push_back(geo::IdiPoint<Point>(*it, (*it).concentration()));
      }
    }
  }
  */

  typedef geo::Point<double, 2> Point;
  typedef double Value;
  typedef geo::PointValue<Point, Value> PointValue;
  std::vector<PointValue> pointValues;

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc)) {
      for(geo::GriddedPoints<Particle>::const_iterator it =
         _particles.begin(*loc); it != _particles.end(*loc); ++it) {
        pointValues.push_back(PointValue(Point(*it), (*it).concentration()));
      }
    }
  }

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    // conc.cell(*loc) = averageConcentration(*loc);
    double sum;

    if(_particles.isMV(*loc)) {
      pcr::setMV(conc.cell(*loc));
    }
    else {
      if(_particles.nrPoints(*loc) == 0) {
        // Empty cell, no points present anymore.
        // Determine concentration based on points in surrounding cells.
        conc.cell(*loc) = concentration(pointValues, *loc);
      }
      else {
        // Determine average concentration of points.
        sum = 0.0;
        for(geo::GriddedPoints<Particle>::const_iterator it =
              _particles.begin(*loc); it != _particles.end(*loc); ++it) {
          PRECOND((*it).concentration() >= 0.0);
          sum += (*it).concentration();
        }

        conc.cell(*loc) = sum / _particles.nrPoints(*loc);
      }

      POSTCOND(conc.cell(*loc) >= 0.0);
    }
  }
}



void pt::ParticleTracker::sumConcentration(
         geo::SimpleRaster<double>& conc) const
{
  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid();
         ++loc) {
    double sum;

    if(_particles.isMV(*loc)) {
      pcr::setMV(conc.cell(*loc));
    }
    else {
      if(_particles.nrPoints(*loc) == 0) {
        conc.cell(*loc) = 0.0;
      }
      else {
        sum = 0.0;
        for(geo::GriddedPoints<Particle>::const_iterator it =
           _particles.begin(*loc); it != _particles.end(*loc); ++it) {
          PRECOND((*it).concentration() >= 0.0);
          sum += (*it).concentration();
        }
        conc.cell(*loc) = sum;
      }

      PRECOND(conc.cell(*loc) >= 0.0);
    }
  }
}



//!
/*!
  \param     conc1 Average concentration at time step k - 1.
  \param     conc2 Average concentration at time step k*.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void pt::ParticleTracker::changeInConcentrations(
         const geo::SimpleRaster<double>& conc1,
         const geo::SimpleRaster<double>& conc2,
         const geo::SimpleRaster<double>& flux,
         const geo::SimpleRaster<double>& head,
         const geo::SimpleRaster<double>& headPrevious,
         const geo::SimpleRaster<double>& satThickness,
         const geo::SimpleRaster<double>& satThicknessPrevious,
         double timeIncrement,
         geo::SimpleRaster<double>& deltaConc) const
{
  PRECOND(timeIncrement > 0.0);
  double halfTimeStep = 0.5 * timeIncrement;
  double factor, gradient1, gradient2;

  for(geo::CellLocVisitor visitor(nrRows(), nrCols()); visitor.valid();
         ++visitor) {
    if(_particles.isMV(*visitor)) {
      PRECOND(pcr::isMV(conc1.cell(*visitor)) &&
              pcr::isMV(conc2.cell(*visitor)));
      pcr::setMV(deltaConc.cell(*visitor));
    }
    else {
      PRECOND(!pcr::isMV(conc1.cell(*visitor)) &&
              !pcr::isMV(conc2.cell(*visitor)) &&
              !pcr::isMV(satThickness.cell(*visitor)));

      factor = halfTimeStep / satThickness.cell(*visitor);
      gradient1 = concentrationGradient(conc1, flux, head, headPrevious,
         satThickness, satThicknessPrevious, timeIncrement, *visitor);
      gradient2 = concentrationGradient(conc2, flux, head, headPrevious,
         satThickness, satThicknessPrevious, timeIncrement, *visitor);
      POSTCOND(!pcr::isMV(gradient1));
      POSTCOND(!pcr::isMV(gradient2));
      deltaConc.cell(*visitor) = factor * gradient1 + factor * gradient2;

      if(deltaConc.cell(*visitor) < 0.0) {

        // TODO: special problem 6
        if(conc2.cell(*visitor) == 0.0) {
          deltaConc.cell(*visitor) = 0.0;
        }

        if(std::abs(deltaConc.cell(*visitor)) > conc2.cell(*visitor)) {
          if(!com::equal_epsilon(deltaConc.cell(*visitor),
              conc2.cell(*visitor))) {
            // Difference is too big.
            Warning("decrease in C (%g) > C* (%g), clamping delta C to C*",
                deltaConc.cell(*visitor), conc2.cell(*visitor));
          }

          deltaConc.cell(*visitor) = -1.0 * conc2.cell(*visitor);
        }

/*
        // TODO: ?
        if(std::abs(deltaConc.cell(*visitor)) > conc2.cell(*visitor)) {
          deltaConc.cell(*visitor) = -1.0 * conc2.cell(*visitor);
        }
*/

        POSTCOND(std::abs(deltaConc.cell(*visitor)) <= conc2.cell(*visitor));
      }
    }
  }
}



double pt::ParticleTracker::concentrationGradient(
         const geo::SimpleRaster<double>& conc,
         const geo::SimpleRaster<double>& flux,
         const geo::SimpleRaster<double>& head,
         const geo::SimpleRaster<double>& headPrevious,
         const geo::SimpleRaster<double>& satThickness,
         const geo::SimpleRaster<double>& satThicknessPrevious,
         double timeIncrement,
         const geo::CellLoc& loc) const
{
  // i: column number
  // j: row number

  double result;

  if(pcr::isMV(conc.cell(loc))) {
    pcr::setMV(result);
  }
  else {
    PRECOND(!pcr::isMV(_disp1.cell(loc)));
    PRECOND(!pcr::isMV(_disp2.cell(loc)));
    PRECOND(!pcr::isMV(_disp3.cell(loc)));
    PRECOND(!pcr::isMV(_disp4.cell(loc)));

    result = 0.0;

    if(  loc.col() < nrCols() - 1 &&
         !pcr::isMV(conc.cell(loc.row(), loc.col() + 1))) {
      result += _disp1.cell(loc) *
         (conc.cell(loc.row(), loc.col() + 1) - conc.cell(loc));
    }

    if(  loc.col() > 0 &&
         !pcr::isMV(_disp1.cell(loc.row(), loc.col() - 1)) &&
         !pcr::isMV(conc.cell(loc.row(), loc.col() - 1))) {
      result -= _disp1.cell(loc.row(), loc.col() - 1) *
         (conc.cell(loc) - conc.cell(loc.row(), loc.col() - 1));
    }

    if(  loc.row() > 0 && loc.row() < nrRows() - 1 &&
         loc.col() < nrCols() - 1 &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col())) &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col() + 1)) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col())) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col() + 1))) {
      result += _disp3.cell(loc) *
         (conc.cell(loc.row() + 1, loc.col()) +
          conc.cell(loc.row() + 1, loc.col() + 1) -
          conc.cell(loc.row() - 1, loc.col()) -
          conc.cell(loc.row() - 1, loc.col() + 1));
    }

    if(  loc.row() > 0 && loc.row() < nrRows() - 1 &&
         loc.col() > 0 &&
         !pcr::isMV(_disp3.cell(loc.row(), loc.col() - 1)) &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col() - 1)) &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col())) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col() - 1)) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col()))) {
      result -= _disp3.cell(loc.row(), loc.col() - 1) *
        (conc.cell(loc.row() + 1, loc.col() - 1) +
         conc.cell(loc.row() + 1, loc.col()) -
         conc.cell(loc.row() - 1, loc.col() - 1) -
         conc.cell(loc.row() - 1, loc.col()));
    }

    if(  loc.row() < nrRows() - 1 &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col()))) {
      result += _disp2.cell(loc) *
        (conc.cell(loc.row() + 1, loc.col()) -
         conc.cell(loc));
    }

    if(  loc.row() > 0 &&
         !pcr::isMV(_disp2.cell(loc.row() - 1, loc.col())) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col()))) {
      result -= _disp2.cell(loc.row() - 1, loc.col()) *
        (conc.cell(loc) -
         conc.cell(loc.row() - 1, loc.col()));
    }

    if(  loc.row() < nrRows() - 1 &&
         loc.col() > 0 && loc.col() < nrCols() - 1 &&
         !pcr::isMV(conc.cell(loc.row(), loc.col() + 1)) &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col() + 1)) &&
         !pcr::isMV(conc.cell(loc.row(), loc.col() - 1)) &&
         !pcr::isMV(conc.cell(loc.row() + 1, loc.col() - 1))) {
      result += _disp4.cell(loc) *
        (conc.cell(loc.row(), loc.col() + 1) +
         conc.cell(loc.row() + 1, loc.col() + 1) -
         conc.cell(loc.row(), loc.col() - 1) -
         conc.cell(loc.row() + 1, loc.col() - 1));
    }

    if(  loc.row() > 0 &&
         loc.col() > 0 && loc.col() < nrCols() - 1 &&
         !pcr::isMV(_disp4.cell(loc.row() - 1, loc.col())) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col() + 1)) &&
         !pcr::isMV(conc.cell(loc.row(), loc.col() + 1)) &&
         !pcr::isMV(conc.cell(loc.row() - 1, loc.col() - 1)) &&
         !pcr::isMV(conc.cell(loc.row(), loc.col() - 1))) {
       result -= _disp4.cell(loc.row() - 1, loc.col()) *
         (conc.cell(loc.row() - 1, loc.col() + 1) +
          conc.cell(loc.row(), loc.col() + 1) -
          conc.cell(loc.row() - 1, loc.col() - 1) -
          conc.cell(loc.row(), loc.col() - 1));
     }

    result += conc.cell(loc) *
      (_storageCoef.cell(loc) *
        (head.cell(loc) - headPrevious.cell(loc)) / timeIncrement +
      flux.cell(loc) -
      _effPorosity.cell(loc) *
        (satThickness.cell(loc) - satThicknessPrevious.cell(loc)) / timeIncrement);

    if(isSourceOrSinkCell(flux, loc)) {
      result -= conc.cell(loc) * flux.cell(loc);
    }

    POSTCOND(!pcr::isMV(result));
  }

  return result;
}



void pt::ParticleTracker::adjustConcentration(
         const geo::SimpleRaster<double>& conc1,
         const geo::SimpleRaster<double>& conc2,
         const geo::SimpleRaster<double>& flux,
         geo::SimpleRaster<double>& deltaConc)
{
  for(geo::CellLocVisitor visitor(nrRows(), nrCols()); visitor.valid();
         ++visitor) {
    if(_particles.isMV(*visitor)) {
      PRECOND(pcr::isMV(conc2.cell(*visitor)) &&
              pcr::isMV(deltaConc.cell(*visitor)));
    }
    else if(isSourceCell(flux, *visitor)) {
      // The concentration of each point in a source cell is updated by setting
      // it equal to the final nodal concentration.
      PRECOND(!pcr::isMV(conc2.cell(*visitor)));
      PRECOND(!pcr::isMV(deltaConc.cell(*visitor)));

      // Special problem 5.
      double concentration = conc2.cell(*visitor) + deltaConc.cell(*visitor);
      POSTCOND(concentration >= 0.0);

      if(_iniConc.cell(*visitor) > conc1.cell(*visitor)) {
        // PRECOND(concentration <= _iniConc.cell(*visitor));
        if(concentration > _iniConc.cell(*visitor)) {
          // TODO: increase precission
          Warning("New concentration in source (%g) should be <= source concentration (%g)",
              concentration, _iniConc.cell(*visitor));
        }
      }
      else if(_iniConc.cell(*visitor) < conc1.cell(*visitor)) {
        // PRECOND(concentration >= _iniConc.cell(*visitor));
        if(concentration < _iniConc.cell(*visitor)) {
          // TODO: increase precission
          Warning("New concentration in source (%g) should be >= source concentration (%g)",
              concentration, _iniConc.cell(*visitor));
        }
      }

      for(geo::GriddedPoints<Particle>::iterator it =
              _particles.begin(*visitor); it != _particles.end(*visitor);
              ++it) {
        (*it).setConcentration(concentration);
      }
    }
    else {
      PRECOND(!pcr::isMV(conc2.cell(*visitor)));
      PRECOND(!pcr::isMV(deltaConc.cell(*visitor)));

      adjustConcentration(*visitor, conc2.cell(*visitor),
         deltaConc.cell(*visitor));

/*
      if(deltaConc.cell(*visitor) >= 0.0) {
        // Concentration increases.
        // Just add the change in concentration to each particle in the cell.
        for(geo::GriddedPoints<Particle>::iterator it =
           _particles.begin(*visitor); it != _particles.end(*visitor); ++it) {
          PRECOND((*it).concentration() >= 0.0);
          (*it).setConcentration((*it).concentration() +
              deltaConc.cell(*visitor));
        }
      }
      else {
        // Concentration decreases.
        if(conc2.cell(*visitor) == 0.0) {
          // If conc2 == 0.0 all concentrations in _particles are 0.0 so we
          // cannot lower them.
          // TODO: increase precission
          Warning("decrease in C (%g) due to dispersion while C* == 0.0, clamping delta C to 0.0",
              deltaConc.cell(*visitor));

          // Do nothing.

#ifdef DEBUG_DEVELOP
          for(geo::GriddedPoints<Particle>::iterator it =
              _particles.begin(*visitor); it != _particles.end(*visitor);
              ++it) {
            PRECOND((*it).concentration() == 0.0);
          }
#endif
        }
        else {
          PRECOND(conc2.cell(*visitor) > 0.0);
          // Substract a percentage of the concentration from each particle
          // which equals the percentage decrease in the cell.
          double percentage = deltaConc.cell(*visitor) / conc2.cell(*visitor);
          POSTCOND(percentage < 0.0);
          POSTCOND(percentage >= -1.0);

          for(geo::GriddedPoints<Particle>::iterator it =
              _particles.begin(*visitor); it != _particles.end(*visitor);
              ++it) {
            PRECOND((*it).concentration() >= 0.0);
            (*it).setConcentration((*it).concentration() +
                percentage * (*it).concentration());
            POSTCOND((*it).concentration() >= 0.0);
          }
        }
      }
*/
    }
  }
}



/*
void pt::ParticleTracker::adjustNetMassFlux(
         const geo::SimpleRaster<double>& conc,
         const geo::SimpleRaster<double>& flux,
         double timeIncrement)
{
  double netMassFlux = 0.0;

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc)) {
      if(isSourceCell(flux, *loc)) {
        // Determine mass generated in source.
        _netMassFlux += flux.cell(*loc) * cellArea() * timeIncrement *
              _iniConc.cell(*loc);
      }
      else if(isSinkCell(flux, *loc)) {
        // Determine mass lost in sink.
        _netMassFlux += flux.cell(*loc) * cellArea() * timeIncrement *
              conc.cell(*loc);
      }
    }
  }

  _netMassFlux += netMassFlux;
}
*/



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void pt::ParticleTracker::moveParticles(
         const geo::SimpleRaster<double>& xVeloc,
         const geo::SimpleRaster<double>& yVeloc,
         const geo::SimpleRaster<double>& flux,
         double timeIncrement)
{
  double dx, dy;
  double xParticle, yParticle, xParticleNew, yParticleNew;
  double xVelocParticle, yVelocParticle;

  double rowNewExact, colNewExact;
  size_t rowNew, colNew;

  double offsetX, offsetY;

  geo::GriddedPoints<Particle> newParticles(_particles.space(),
         _particles.missingValues());
  Particle newParticle, replaceParticle;

  // Loop over all cells.
  for(size_t row = 0; row < nrRows(); ++row) {
    for(size_t col = 0; col < nrCols(); ++col) {

      if(!_particles.isMV(row, col)) {

        // Loop over all particles in each cell.
        for(geo::GriddedPoints<Particle>::iterator it =
             _particles.begin(row, col); it != _particles.end(row, col);
             ++it) {

          // Select new particle.
          // --> We have a particle now.

          // Is particle in grid?
          // --> All selected particles are in the grid.

          // Determine x-y coordinates of node where particle is located.
          // --> This is maybe not needed.

          // Is cell in aquifer?
          // --> All selected particles are in the aquifer.

          // Determine in which quadrant of cell the particle is located.
          xParticle = (*it)[0];
          yParticle = (*it)[1];
          geo::Quadrant quadrant =
                _particles.space().quadrant(xParticle, yParticle);

          // Is quadrant located in or adjacent to a source or sink?
          // Set velocity at source / sink node = velocity on adjacent cell
          // boundary.
          // ...

          // Use bilinear interpolation to compute x and y velocity of particle.
          velocity(xVeloc, yVeloc, flux, row, col, quadrant, xVelocParticle,
                yVelocParticle);
          POSTCOND(!pcr::isMV(xVelocParticle));
          POSTCOND(!pcr::isMV(yVelocParticle));

          // Compute distance moved in x and y directions.
          dx = timeIncrement * xVelocParticle;
          dy = timeIncrement * yVelocParticle;
          // According to criterium three:
          POSTCOND(dx <= _gamma * cellSize());
          POSTCOND(dy <= _gamma * cellSize());

          // Computate x and y coordinates of cell at new location of particle.
          xParticleNew = xParticle + dx;
          yParticleNew = yParticle + dy;

          // Special problem 1.
          // Is new particle outside aquifer boundary?
          if(!inAquifer(xParticleNew, yParticleNew)) {

            // Compute distance particle traveled beyond boundary.
            // Relocate particle into aquifer by reflection across boundary.
            reflect(row, col, xParticleNew, yParticleNew);

            if(!inAquifer(xParticleNew, yParticleNew)) {
              // This happens when the new location of the particle is either
              // to the ul, ur, lr or ll of the current cell and the particle
              // is reflected to a adjecent cell which also is not in the
              // aquifer. By reflecting these particles a second time they are
              // reflected into the current cell.
              reflect(row, col, xParticleNew, yParticleNew);
            }

            POSTCOND(inAquifer(xParticleNew, yParticleNew));
          }

          // Adding the new particle to the _particles variable will
          // invalidate the iterators we currently use! New particles are
          // stored in a temp raster.
          newParticle = *it;
          newParticle[0] = xParticleNew;
          newParticle[1] = yParticleNew;
          newParticles.insert(newParticle);

          // Sum number of particles and concentrations in cell at new location.
          // ...

          // Has particle changed cell location?
          coords2RowCol(xParticleNew, yParticleNew, rowNewExact, colNewExact);
          PRECOND(rowNewExact >= 0.0 && rowNewExact < nrRows());
          PRECOND(colNewExact >= 0.0 && colNewExact < nrCols());
          rowNew = static_cast<size_t>(std::floor(rowNewExact));
          colNew = static_cast<size_t>(std::floor(colNewExact));
          if(rowNew != row || colNew != col) {

            // Is old location in a source cell or a sink cell?
            if(isSourceOrSinkCell(flux, row, col)) {

              // Special problem 3 and 4.
              // Create new particle.
              replaceParticle = *it;

              // Source or sink?
              if(isSourceCell(flux, row, col)) {

                // Did particle originate in that source cell?
                if(newParticle.born(row, col)) {

                  // Is source cell located along edge of aquifer?
                  if(cellAlongEdgeAquifer(row, col)) {

                    // Place new particle in old cell at same relative position
                    // as old particle in new cell.
                    offsetX = colNewExact - std::floor(colNewExact);
                    offsetY = rowNewExact - std::floor(rowNewExact);
                    POSTCOND(offsetX >= 0.0 && offsetX < 1.0);
                    POSTCOND(offsetY >= 0.0 && offsetY < 1.0);
                    rowCol2Coords(row + offsetY, col + offsetX, xParticleNew,
                        yParticleNew);
                    replaceParticle[0] = xParticleNew;
                    replaceParticle[1] = yParticleNew;
                    replaceParticle.setConcentration(_iniConc.cell(row, col));
                    newParticles.insert(replaceParticle);
                  }
                  else {

                    // Place particle at original location of old particle.
                    replaceParticle.setConcentration(_iniConc.cell(row, col));
                    newParticles.insert(replaceParticle);
                  }
                }
              }
              else {

                // Place new particle in old cell at same relative position
                // as old particle in new cell.
                offsetX = colNewExact - std::floor(colNewExact);
                offsetY = rowNewExact - std::floor(rowNewExact);
                POSTCOND(offsetX >= 0.0 && offsetX < 1.0);
                POSTCOND(offsetY >= 0.0 && offsetY < 1.0);
                rowCol2Coords(row + offsetY, col + offsetX, xParticleNew,
                    yParticleNew);
                replaceParticle[0] = xParticleNew;
                replaceParticle[1] = yParticleNew;
                newParticles.insert(replaceParticle);
              }
            }

/*
            // MOVED TO OTHER LOCATION
            // Is new location in a pumping or constant-head sink?
            if(isSinkCell(flux, rowNew, colNew)) {
              // Remove particle from grid.
              newParticles.remove(newParticle, rowNew, colNew);
            }
*/
          }
        }

        // Call subroutine CNCON to compute new concentrations.
        // ...

        // Store observation well data for steady-flow cases.
        // ...
      }
    }
  }

  _particles = newParticles;
  POSTCOND(_particles.nrPoints() == newParticles.nrPoints());
}



//! Calculates the concentration at each cell.
/*!
  \param     xVeloc Velocity in x direction at cell center.
  \param     yVeloc Velocity in y direction at cell center.
  \param     longDisp Longitudinal dispersivity.
  \param     transDisp Transverse dispersivity.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void pt::ParticleTracker::calculateConcentration(
         const geo::SimpleRaster<double>& flux,
         const geo::SimpleRaster<double>& xVeloc,
         const geo::SimpleRaster<double>& yVeloc,
         const geo::SimpleRaster<double>& longDisp,
         const geo::SimpleRaster<double>& transDisp,
         const geo::SimpleRaster<double>& head,
         const geo::SimpleRaster<double>& satThickness)
{
  // Check whether all datasets conform to the extent of the aquifer.
  checkWithAquifer(flux);
  checkWithAquifer(xVeloc);
  checkWithAquifer(yVeloc);
  checkWithAquifer(longDisp);
  checkWithAquifer(transDisp);
  checkWithAquifer(head);
  checkWithAquifer(satThickness);

  static size_t timeStep = 0;
  ++timeStep;
  if(timeStep == 1) {
    _initialMass = mass(_iniConc, satThickness);
  }

  {
    geo::SimpleRaster<double> conc(nrRows(), nrCols());
    averageConcentration(conc);

/*
    std::cout
       << percentageMassError3(timeIncrement, conc, flux, satThickness)
       << '\t' << massGenerated(timeIncrement, flux)
       << '\t' << massLost(timeIncrement, flux)
       << std::endl;
       */

    /*
    std::ofstream stream;
    com::open(stream,"out" + com::toString(timeStep));
    for(size_t col = 0; col < nrCols(); ++col) {
      stream << col << '\t' << mass(col, conc, satThickness) << std::endl;
    }
    */
  }

  // Velo
  calculateVelocity(xVeloc, yVeloc);

  size_t nrMoves;
  double timeIncrement;

  {
    geo::RasterBoundaries<double> Dxx(nrRows(), nrCols());
    geo::RasterBoundaries<double> Dyy(nrRows(), nrCols());
    calculateDispersionEquationCoefficients(longDisp, transDisp, satThickness,
         Dxx, Dyy);
    nrMoves = calculateNumberOfParticleMoves(xVeloc, yVeloc, flux,
         satThickness, Dxx, Dyy);
    POSTCOND(nrMoves > 0);
    timeIncrement = this->timeIncrement() / nrMoves;
    POSTCOND(timeIncrement > 0.0);
  }

  for(size_t i = 0; i < nrMoves; ++i) {

    // Compute elapsed time at start of next particle movement.
    // ...

    // Determine average concentration of all points within the area of the
    // cells (before all points have been moved by advective transport).
    geo::SimpleRaster<double> conc1(nrRows(), nrCols());
    averageConcentration(conc1);

    moveParticles(xVeloc, yVeloc, flux, timeIncrement);

    // Generate new particles or remove old particles at appropiate boundaries.
    // ...

    // Compute average concentration in each finite difference cell.
    // Determine average concentration of all points within the area of the
    // cells (now that all points have been moved by advective transport).
    geo::SimpleRaster<double> conc2(nrRows(), nrCols());
    averageConcentration(conc2);

    // TODO
    geo::SimpleRaster<double> headPrevious = head;
    geo::SimpleRaster<double> satThicknessPrevious = head;

    // Compute explicitly the chemical concentration at nodes.
    geo::SimpleRaster<double> deltaConc(nrRows(), nrCols());
    changeInConcentrations(conc1, conc2, flux, head, headPrevious,
         satThickness, satThicknessPrevious, timeIncrement, deltaConc);

    // Adjust concentration of each particle.
    adjustConcentration(conc1, conc2, flux, deltaConc);

    // Compute mass balance.
    // ...

    updateDistribution(flux);
  }
}



void pt::ParticleTracker::concentration(geo::SimpleRaster<double>& conc) const
{
  averageConcentration(conc);
}



void pt::ParticleTracker::nrParticles(
         geo::SimpleRaster<UINT4>& nrParticles)
{
  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {

    if(!_particles.isMV(*loc)) {
      nrParticles.cell(*loc) = _particles.nrPoints(*loc);
    }
    else {
      pcr::setMV(nrParticles.cell(*loc));
    }
  }
}



//! Calculates the total mass in the area.
/*!
  \param     conc Current concentrations.
  \param     satThicknessPrevious Saturated thickness.
  \return    Mass.
*/
double pt::ParticleTracker::mass(
         const geo::SimpleRaster<double>& conc,
         const geo::SimpleRaster<double>& satThickness) const
{
  double mass = 0.0;

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {

    if(!_particles.isMV(*loc)) {
      PRECOND(!pcr::isMV(conc.cell(*loc)));
      PRECOND(!pcr::isMV(satThickness.cell(*loc)));
      PRECOND(!pcr::isMV(_effPorosity.cell(*loc)));
      mass += satThickness.cell(*loc) * _effPorosity.cell(*loc) *
         cellArea() * conc.cell(*loc);
    }
  }

  return mass;
}



double pt::ParticleTracker::mass(
         size_t col,
         const geo::SimpleRaster<double>& conc,
         const geo::SimpleRaster<double>& satThickness) const
{
  double mass = 0.0;

  for(size_t row = 0; row < nrRows(); ++row) {

    if(!_particles.isMV(row, col)) {
      PRECOND(!pcr::isMV(conc.cell(row, col)));
      PRECOND(!pcr::isMV(satThickness.cell(row, col)));
      PRECOND(!pcr::isMV(_effPorosity.cell(row, col)));
      mass += satThickness.cell(row, col) * _effPorosity.cell(row, col) *
         cellArea() * conc.cell(row, col);
    }
  }

  return mass;
}



//! Calculates the amount of mass generated in the source cells.
/*!
  \param     timeIncrement Time increment.
  \param     flux Fluxes.
  \return    Mass.
  \sa        massLost(double, const geo::SimpleRaster<double>&)

  The calculated mass a constant per time increment.
*/
double pt::ParticleTracker::massGenerated(double timeIncrement,
         const geo::SimpleRaster<double>& flux) const
{
  double mass = 0.0;

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc) && isSourceCell(flux, *loc)) {
      PRECOND(!pcr::isMV(_iniConc.cell(*loc)));

      mass += flux.cell(*loc) * cellArea() * timeIncrement *
         _iniConc.cell(*loc);
    }
  }

  return mass;
}



//! Calculates the amount of mass lost in sink cells.
/*!
  \param     timeIncrement Time increment.
  \param     flux Fluxes.
  \return    Mass.
  \sa        massGenerated(double, const geo::SimpleRaster<double>&)

  The calculated mass a constant per time increment.
*/
double pt::ParticleTracker::massLost(double timeIncrement,
         const geo::SimpleRaster<double>& flux) const
{
  double mass = 0.0;

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc) && isSinkCell(flux, *loc)) {
      PRECOND(!pcr::isMV(_iniConc.cell(*loc)));

      mass += flux.cell(*loc) * cellArea() * timeIncrement *
         _iniConc.cell(*loc);
    }
  }

  return -mass;
}



//! Calculates the netto mass flux.
/*!
  \param     timeIncrement Time increment.
  \param     flux Fluxes.
  \return    Mass.
*/
double pt::ParticleTracker::netMassFlux(double timeIncrement,
         const geo::SimpleRaster<double>& flux) const
{
  return massGenerated(timeIncrement, flux) - massLost(timeIncrement, flux);
}



//! Calculates the different between the current mass en the initial mass.
/*!
  \param     conc Current concentration.
  \param     conc Saturated thickness.
  \return    Mass.
*/
double pt::ParticleTracker::changeInMass(
         const geo::SimpleRaster<double>& conc,
         const geo::SimpleRaster<double>& satThickness) const
{
  return mass(conc, satThickness) - _initialMass;
}



//! Calculates the percent error in the mass balance, variant 3.
/*!
  \param     timeIncrement Time increment.
  \param     conc Current concentration.
  \param     flux Flux.
  \param     satThickness Saturated thickness.
  \return    Percent error.

  When the influx equals the outflux this measure equals the percentage change
  in mass relative to the initial situation (positive value meens los of mass).
  When this value becomes constant no mass is lost or generated.
*/
double pt::ParticleTracker::percentageMassError3(double timeIncrement,
         const geo::SimpleRaster<double>& conc,
         const geo::SimpleRaster<double>& flux,
         const geo::SimpleRaster<double>& satThickness) const
{
  double netMassFlux = this->netMassFlux(timeIncrement, flux);
  double changeInMass = this->changeInMass(conc, satThickness);

  PRECOND(_initialMass != netMassFlux);

  double error = 100.0 * (netMassFlux - changeInMass) /
         (_initialMass - netMassFlux);

  return error;
}



//! Calculates the percentage of cells without particles.
/*!
  \return    Percentage.
*/
double pt::ParticleTracker::percentageVoidCells() const
{
  size_t nrVoidCells = 0;
  size_t nrCells = 0;

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc)) {
      if(!_particles.size(*loc)) {
        ++nrVoidCells;
      }
      ++nrCells;
    }
  }

  return nrCells ? 100.0 * static_cast<double>(nrVoidCells) /
         static_cast<double>(nrCells): 0.0;
}



//! Adjusts the concentration at \a loc by \a deltaConc.
/*!
  \param     loc Current cell location.
  \param     conc Average concentration at current location.
  \param     deltaConc Change in concentration.
  \warning   This function assumes that \a conc is not a missing value and that in case \a deltaConc is negative, it is not larger than \a conc.
*/
void pt::ParticleTracker::adjustConcentration(const geo::CellLoc& loc,
         double conc, double deltaConc)
{
  if(deltaConc >= 0.0) {
    // Concentration increases.
    // Just add the change in concentration to each particle in the cell.
    for(geo::GriddedPoints<Particle>::iterator it = _particles.begin(loc);
         it != _particles.end(loc); ++it) {
      PRECOND((*it).concentration() >= 0.0);
      (*it).setConcentration((*it).concentration() + deltaConc);
    }
  }
  else {
    // Substract a percentage of the concentration from each particle
    // which equals the percentage decrease in the cell.

    POSTCOND(!pcr::isMV(conc) && conc >= 0.0);

    if(conc == 0.0) {
      // If conc == 0.0 all concentrations in _particles are 0.0 so we
      // cannot lower them.
      // TODO: increase precission
      Warning("decrease in C (%g) while C == 0.0, discarding delta C",
         deltaConc);

      // Do nothing.

#ifdef DEBUG_DEVELOP
      for(geo::GriddedPoints<Particle>::iterator it =
         _particles.begin(loc); it != _particles.end(loc); ++it) {
        PRECOND((*it).concentration() == 0.0);
      }
#endif
    }
    else {
      double percentage = deltaConc / conc;
      POSTCOND(percentage < 0.0);
      POSTCOND(percentage >= -1.0);

      for(geo::GriddedPoints<Particle>::iterator it =
          _particles.begin(loc); it != _particles.end(loc); ++it) {
        PRECOND((*it).concentration() >= 0.0);
        (*it).setConcentration((*it).concentration() +
            percentage * (*it).concentration());
        POSTCOND((*it).concentration() >= 0.0);
      }
    }
  }
}



/*
void pt::ParticleTracker::adjustConcentration(
         const geo::SimpleRaster<double>& deltaConc)
{
  geo::SimpleRaster<double> conc(nrRows(), nrCols());
  averageConcentration(conc);

  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid();
         ++loc) {
    if(!_particles.isMV(*loc) && !pcr::isMV(deltaConc.cell(*loc))) {
      adjustConcentration(*loc, conc.cell(*loc), deltaConc.cell(*loc));
    }
  }
}
*/



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Doen we percentage maal de gemiddelde concentratie of percentage
             maal de individuele concentraties? Nu het laatste.
*/
void pt::ParticleTracker::adjustConcentration(
         const geo::SimpleRaster<double>& adjustConc)
{
  for(geo::CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    if(!_particles.isMV(*loc) && !pcr::isMV(adjustConc.cell(*loc))) {
      for(geo::GriddedPoints<Particle>::iterator it =
          _particles.begin(*loc); it != _particles.end(*loc); ++it) {
        PRECOND((*it).concentration() >= 0.0);
        (*it).setConcentration((*it).concentration() +
            adjustConc.cell(*loc) * (*it).concentration());
        POSTCOND((*it).concentration() >= 0.0);
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



// Explicit template instantiation.
template class geo::GriddedPoints<pt::Particle>;
