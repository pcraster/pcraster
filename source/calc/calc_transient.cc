#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC
#include "calc.h"
#define INCLUDED_CALC
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif

#ifndef INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#include "fieldapi_scalardomaincheck.h"
#define INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

// Module headers.



//------------------------------------------------------------------------------
// Implicit finite difference according to:
// Introduction to groundwater modeling
// Wang & Anderson
// Crank-Nicolson method, Gauss-Seidel iteration
//------------------------------------------------------------------------------

namespace calc {

struct CalculationArgs {

  double d_area;     // Cell area, constant.
  double d_alpha;    // Constant.
  double d_nrCells;  // Number of surrounding cells contributing to current cel.
  double d_interval;
  double d_currentElevation;
  double d_recharge;
  double d_transmissivity;   // Average t for all directions.
  double d_storageCoefficient;
  double d_h1;       // h * t for all directions, timestep n
  double d_h2;       // h * t for all directions, timestep n + 1.

};



double calcElevation(const CalculationArgs& args)
{
  double f1 = (args.d_area * args.d_storageCoefficient) / args.d_interval;
  double f2 = 1.0 / (f1 + args.d_nrCells * args.d_transmissivity *
                   args.d_alpha);

  return f2 * (args.d_alpha * args.d_h2 + f1 * args.d_currentElevation +
                   (1.0 - args.d_alpha) *
                   (args.d_h1 - args.d_currentElevation * args.d_nrCells *
                   args.d_transmissivity) + args.d_area * args.d_recharge);
}



class DiffuseAlgorithm: public boost::noncopyable 
{

public:

  enum Direction {
    NoDirection = 0x000,
    Left = 0x0001,
    Top = 0x0002,
    Right = 0x0004,
    Bottom = 0x0008,
    UpperLeft = Top | Left,
    UpperRight = Top | Right,
    LowerLeft = Bottom | Left,
    LowerRight = Bottom | Right,
    Enclosed = Left | Top | Right | Bottom
  };

  enum FlowCondition  {
    NOFLOW = 0,
    CALCULATE = 1,
    FIXED = 2
  };


private:

  fieldapi::ReadWriteReal8&  d_resultElevation;
  const fieldapi::ReadOnlyReal8& d_elevation;
  const fieldapi::ReadOnlyReal8& d_recharge;
  const fieldapi::ReadOnlyReal8& d_transmissivity;
  const fieldapi::ReadOnlyInt4& d_flowCondition;
  const fieldapi::ReadOnlyReal8& d_storageCoefficient;

  CalculationArgs d_args;
  geo::CellLoc d_location;    // A cell location.

  geo::SimpleRaster<double> d_tmRight; // Average transm. values, right.
  geo::SimpleRaster<double> d_tmBottom; // Average transm. values, bottom.

  geo::SimpleRaster<unsigned int> d_noFlowBoundaries;



  double calcElevationWithNoFlowBoundary(size_t r, size_t c, unsigned int direction)
  {
    if(direction == Enclosed) {
      // Current cell is surrounded by no flow boundaries. Current elevation
      // remains unchanged.
      return d_elevation.value(r, c);
    }

    // Calculate new elevation.
    d_args.d_h1 = 0.0;
    d_args.d_h2 = 0.0;
    d_args.d_nrCells = 0;

    // Harmonic means of transmissivities.
    double tmLeft(0), tmTop(0), tmRight(0), tmBottom(0);

    // Cell to the left.
    if(!(direction & Left)) {
      tmLeft = d_tmRight.cell(r, c - 1);
      d_args.d_h1 += tmLeft * d_elevation.value(r, c - 1);
      d_args.d_h2 += tmLeft * d_resultElevation.value(r, c - 1);
      ++d_args.d_nrCells;
    }
    else if(!(direction & Right)) {
      // Copy value from Right cell.
      tmLeft = d_tmRight.cell(r, c);
      d_args.d_h1 += tmLeft * d_elevation.value(r, c + 1);
      d_args.d_h2 += tmLeft * d_resultElevation.value(r, c + 1);
      ++d_args.d_nrCells;
    }

    // Cell to the top.
    if(!(direction & Top)) {
      tmTop = d_tmBottom.cell(r - 1, c);
      d_args.d_h1 += tmTop * d_elevation.value(r - 1, c);
      d_args.d_h2 += tmTop * d_resultElevation.value(r - 1, c);
      ++d_args.d_nrCells;
    }
    else if(!(direction & Bottom)) {
      // Copy value from Bottom cell.
      tmTop = d_tmBottom.cell(r, c);
      d_args.d_h1 += tmTop * d_elevation.value(r + 1, c);
      d_args.d_h2 += tmTop * d_resultElevation.value(r + 1, c);
      ++d_args.d_nrCells;
    }

    // Cell to the right.
    if(!(direction & Right)) {
      tmRight = d_tmRight.cell(r, c);
      d_args.d_h1 += tmRight * d_elevation.value(r, c + 1);
      d_args.d_h2 += tmRight * d_resultElevation.value(r, c + 1);
      ++d_args.d_nrCells;
    }
    else if(!(direction & Left)) {
      // Copy value from Left cell.
      tmRight = d_tmRight.cell(r, c - 1);
      d_args.d_h1 += tmRight * d_elevation.value(r, c - 1);
      d_args.d_h2 += tmRight * d_resultElevation.value(r, c - 1);
      ++d_args.d_nrCells;
    }

    // Cell to the bottom.
    if(!(direction & Bottom)) {
      tmBottom = d_tmBottom.cell(r, c);
      d_args.d_h1 += tmBottom * d_elevation.value(r + 1, c);
      d_args.d_h2 += tmBottom * d_resultElevation.value(r + 1, c);
      ++d_args.d_nrCells;
    }
    else if(!(direction & Top)) {
      // Copy value from Top cell.
      tmBottom = d_tmBottom.cell(r - 1, c);
      d_args.d_h1 += tmBottom * d_elevation.value(r - 1, c);
      d_args.d_h2 += tmBottom * d_resultElevation.value(r - 1, c);
      ++d_args.d_nrCells;
    }

    PRECOND(d_args.d_nrCells > 0);

    d_args.d_currentElevation = d_elevation.value(r, c);
    d_args.d_recharge = d_recharge.value(r, c);
    // Arithmic mean.
    d_args.d_transmissivity = (tmLeft + tmTop + tmRight + tmBottom) /
                   d_args.d_nrCells;
    d_args.d_storageCoefficient = d_storageCoefficient.value(r, c);

    return calc::calcElevation(d_args);
  }



  double calcElevationWithoutNoFlowBoundary(size_t r, size_t c)
  {
    d_args.d_h1 = 0.0;       // Elevation at t = n, * transmissivities.
    d_args.d_h2 = 0.0;       // Elevation at t = n + 1, * transmissivities.
    d_args.d_nrCells = 0;    // Nr of cells contributing to the current value.
    static double tmLeft, tmTop, tmRight, tmBottom; // Harm. means of transm.

    // The current cell has no no flow boundaries. All neighbours have
    // valid values.

    // Cell to the left.
    tmLeft = d_tmRight.cell(r, c - 1);
    d_args.d_h1 += tmLeft * d_elevation.value(r, c - 1);
    d_args.d_h2 += tmLeft * d_resultElevation.value(r, c - 1);
    ++d_args.d_nrCells;

    // Cell to the top.
    tmTop = d_tmBottom.cell(r - 1, c);
    d_args.d_h1 += tmTop * d_elevation.value(r - 1, c);
    d_args.d_h2 += tmTop * d_resultElevation.value(r - 1, c);
    ++d_args.d_nrCells;

    // Cell to the right.
    tmRight = d_tmRight.cell(r, c);
    d_args.d_h1 += tmRight * d_elevation.value(r, c + 1);
    d_args.d_h2 += tmRight * d_resultElevation.value(r, c + 1);
    ++d_args.d_nrCells;

    // Cell to the bottom.
    tmBottom = d_tmBottom.cell(r, c);
    d_args.d_h1 += tmBottom * d_elevation.value(r + 1, c);
    d_args.d_h2 += tmBottom * d_resultElevation.value(r + 1, c);
    ++d_args.d_nrCells;

    PRECOND(d_args.d_nrCells > 0);
    PRECOND(d_args.d_nrCells == 4);

    d_args.d_currentElevation = d_elevation.value(r, c);
    d_args.d_recharge = d_recharge.value(r, c);
    // Arithmic mean.
    d_args.d_transmissivity = (tmLeft + tmTop + tmRight + tmBottom) /
                   d_args.d_nrCells;
    d_args.d_storageCoefficient = d_storageCoefficient.value(r, c);

    return calc::calcElevation(d_args);
  }



public:

  DiffuseAlgorithm(fieldapi::ReadWriteReal8& resultElevation,
         const fieldapi::ReadOnlyReal8& elevation,
         const fieldapi::ReadOnlyReal8& recharge,
         const fieldapi::ReadOnlyReal8& transmissivity,
         const fieldapi::ReadOnlyInt4& flowCondition,
         const fieldapi::ReadOnlyReal8& storageCoefficient,
         double area, double alpha, double interval)

    : d_resultElevation(resultElevation), d_elevation(elevation),
      d_recharge(recharge), d_transmissivity(transmissivity),
      d_flowCondition(flowCondition), d_storageCoefficient(storageCoefficient),
      d_tmRight(resultElevation.nrRows(), resultElevation.nrCols()),
      d_tmBottom(resultElevation.nrRows(), resultElevation.nrCols()),
      d_noFlowBoundaries(resultElevation.nrRows(), resultElevation.nrCols(),
      static_cast<const unsigned int>(NoDirection))

  {
    d_args.d_area = area;
    d_args.d_alpha = alpha;
    d_args.d_interval = interval;




    geo::CellLoc loc;
    size_t r, c; // comes from C ya know

    //--------------------------------------------------------------------------
    // Calculate transmissivities. These stay constant within a call to the
    // function and are needed for each iteration so we calculate them here
    // once. We trade memory space for speed.

    // Harmonic mean transmissivities current and right cell.
    // For every row.
    for( r = 0; r < d_tmRight.nrRows(); ++r) {

      loc.setRow(r);

      // For every but the last col.
      for( c = 0; c < d_tmRight.nrCols() - 1; ++c) {

        loc.setCol(c);

        if(!d_transmissivity.isMV(loc) &&
                   !d_transmissivity.isMV(geo::CellLoc(r, c + 1))) {
          d_tmRight.cell(r, c) =
              2 * d_transmissivity.value(r, c + 1) *
              d_transmissivity.value(r, c) /
              (d_transmissivity.value(r, c + 1) + d_transmissivity.value(r, c));
        }
      }
    }

    // Harmonic mean transmissivities current and bottom cell.
    // For every but the last row.
    for( r = 0; r < d_tmBottom.nrRows() - 1; ++r) {

      loc.setRow(r);

      // For every col.
      for( c = 0; c < d_tmBottom.nrCols(); ++c) {

        loc.setCol(c);

        if(!d_transmissivity.isMV(loc) &&
                   !d_transmissivity.isMV(geo::CellLoc(r + 1, c))) {
          d_tmBottom.cell(r, c) =
              2 * d_transmissivity.value(r + 1, c) *
              d_transmissivity.value(r, c) /
              (d_transmissivity.value(r + 1, c) + d_transmissivity.value(r, c));
        }
      }
    }

    //--------------------------------------------------------------------------
    // Calculate no flow boundaries. These stay constant within a call to the
    // function and are needed for each iteration so we calculate them here
    // once. We trade memory space for speed.
    // d_noFlowBoundaries is already initialized with NoDirection.
    // A bounding cell is regarded as a now flow boundary if:
    // - it is missing (border cell of the raster)
    // - it contains a missing value
    // - if its flow condition is NOFLOW

    PRECOND(d_noFlowBoundaries.nrRows() >= 1);
    PRECOND(d_noFlowBoundaries.nrCols() >= 1);

    // For every but the first and last row.
    for( r = 1; r < d_noFlowBoundaries.nrRows() - 1; ++r) {

      // For every but the first and last col.
      for( c = 1; c < d_noFlowBoundaries.nrCols() - 1; ++c) {

        // Cell to the left.
        loc.setRow(r);
        loc.setCol(c - 1);
        if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c - 1) == NOFLOW) {
          d_noFlowBoundaries.cell(r, c) |= Left;
        }

        // Cell to the top.
        loc.setRow(r - 1);
        loc.setCol(c);
        if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r - 1, c) == NOFLOW) {
          d_noFlowBoundaries.cell(r, c) |= Top;
        }

        // Cell to the right.
        loc.setRow(r);
        loc.setCol(c + 1);
        if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c + 1) == NOFLOW) {
          d_noFlowBoundaries.cell(r, c) |= Right;
        }

        // Cell to the bottom.
        loc.setRow(r + 1);
        loc.setCol(c);
        if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r + 1, c) == NOFLOW) {
          d_noFlowBoundaries.cell(r, c) |= Bottom;
        }
      }
    }

    // Handle no-flow conditions on the borders of the area. The value of head
    // along the fictious column reflects across the border.


    // Process borders of raster.

    // First col.
    c = 0;

    // For every but the first and last row.
    for(r = 1; r < d_noFlowBoundaries.nrRows() - 1; ++r) {

      // Cell to the left.
      d_noFlowBoundaries.cell(r, c) |= Left;

      // Cell to the top.
      loc.setRow(r - 1);
      loc.setCol(c);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r - 1, c) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Top;
      }

      // Cell to the right.
      loc.setRow(r);
      loc.setCol(c + 1);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c + 1) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Right;
      }

      // Cell to the bottom.
      loc.setRow(r + 1);
      loc.setCol(c);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                 d_flowCondition.value(r + 1, c) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Bottom;
      }
    }

    // Last col.
    c = d_noFlowBoundaries.nrCols() - 1;

    // For every but the first and last row.
    for(r = 1; r < d_noFlowBoundaries.nrRows() - 1; ++r) {

      // Cell to the left.
      loc.setRow(r);
      loc.setCol(c - 1);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c - 1) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Left;
      }

      // Cell to the top.
      loc.setRow(r - 1);
      loc.setCol(c);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r - 1, c) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Top;
      }

      // Cell to the right.
      d_noFlowBoundaries.cell(r, c) |= Right;

      // Cell to the bottom.
      loc.setRow(r + 1);
      loc.setCol(c);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                 d_flowCondition.value(r + 1, c) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Bottom;
      }
    }

    // First row.
    r = 0;

    // For every but the first and last col.
    for(c = 1; c < d_noFlowBoundaries.nrCols() - 1; ++c) {

      // Cell to the left.
      loc.setRow(r);
      loc.setCol(c - 1);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c - 1) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Left;
      }

      // Cell to the top.
      d_noFlowBoundaries.cell(r, c) |= Top;

      // Cell to the right.
      loc.setRow(r);
      loc.setCol(c + 1);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c + 1) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Right;
      }

      // Cell to the bottom.
      loc.setRow(r + 1);
      loc.setCol(c);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                 d_flowCondition.value(r + 1, c) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Bottom;
      }
    }

    // Last row.
    r = d_noFlowBoundaries.nrRows() - 1;

    // For every but the first and last col.
    for(c = 1; c < d_noFlowBoundaries.nrCols() - 1; ++c) {

      // Cell to the left.
      loc.setRow(r);
      loc.setCol(c - 1);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c - 1) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Left;
      }

      // Cell to the top.
      loc.setRow(r - 1);
      loc.setCol(c);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r - 1, c) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Top;
      }

      // Cell to the right.
      loc.setRow(r);
      loc.setCol(c + 1);
      if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c + 1) == NOFLOW) {
        d_noFlowBoundaries.cell(r, c) |= Right;
      }

      // Cell to the bottom.
      d_noFlowBoundaries.cell(r, c) |= Bottom;
    }

    // Upper left corner.
    r = 0;
    c = 0;

    d_noFlowBoundaries.cell(r, c) |= UpperLeft;

    // Cell to the right.
    loc.setRow(r);
    loc.setCol(c + 1);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c + 1) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Right;
    }

    // Cell to the bottom.
    loc.setRow(r + 1);
    loc.setCol(c);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                 d_flowCondition.value(r + 1, c) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Bottom;
    }

    // Upper right corner.
    r = 0;
    c = d_noFlowBoundaries.nrCols() - 1;

    d_noFlowBoundaries.cell(r, c) |= UpperRight;

    // Cell to the left.
    loc.setRow(r);
    loc.setCol(c - 1);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c - 1) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Left;
    }

    // Cell to the bottom.
    loc.setRow(r + 1);
    loc.setCol(c);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                 d_flowCondition.value(r + 1, c) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Bottom;
    }

    // Lower right corner.
    r = d_noFlowBoundaries.nrRows() - 1;
    c = d_noFlowBoundaries.nrCols() - 1;

    d_noFlowBoundaries.cell(r, c) |= LowerRight;

    // Cell to the left.
    loc.setRow(r);
    loc.setCol(c - 1);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c - 1) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Left;
    }

    // Cell to the top.
    loc.setRow(r - 1);
    loc.setCol(c);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r - 1, c) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Top;
    }

    // Lower left corner.
    r = d_noFlowBoundaries.nrRows() - 1;
    c = 0;

    d_noFlowBoundaries.cell(r, c) |= LowerLeft;

    // Cell to the top.
    loc.setRow(r - 1);
    loc.setCol(c);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r - 1, c) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Top;
    }

    // Cell to the right.
    loc.setRow(r);
    loc.setCol(c + 1);
    if(d_elevation.isMV(loc) || d_flowCondition.isMV(loc) ||
                   d_flowCondition.value(r, c + 1) == NOFLOW) {
      d_noFlowBoundaries.cell(r, c) |= Right;
    }
  }



//! Calculates the new elevation at a cell position.
/*!
  \param     r Row of cell.
  \param     c Column of cell.
  \return    Elevation.
  \warning   All input variables and d_resultElevation must have a valid value
             at \a r, \a c. This is forced by checking d_resultElevation.
*/
  double calcElevation(size_t r, size_t c)
  {
    PRECOND(!d_resultElevation.isMV(geo::CellLoc(r, c)));

    if(d_noFlowBoundaries.cell(r, c) != NoDirection) {
      return calcElevationWithNoFlowBoundary(r, c, d_noFlowBoundaries.cell(r, c));
    }
    else {
      return calcElevationWithoutNoFlowBoundary(r, c);
    }
  }

};



} // namespace calc.


//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Elevation                  scalar [ L       ]
  Recharge                   scalar [ L , T-1 ]
  Transmissivity             scalar [ L2, T-1 ]
  Storage coefficient        scalar [ L3, L-3 ]
  Interval                   scalar [ T       ]       > 0.0

  Issues: cell length, error messages and handling of return values
*/
extern "C" int  Transient(void** out, const void** in, int nrArgs)
{
  using namespace calc; // hack hack
  PRECOND(nrArgs == 7);
  (void)nrArgs; // shut up compiler

  ReadWriteReal8_ref(resultElevation, static_cast<MAP_REAL8*>(out[0]));
  ReadOnlyReal8_ref(elevation, static_cast<const MAP_REAL8*>(in[0]));
  ReadOnlyReal8_ref(recharge, static_cast<const MAP_REAL8*>(in[1]));
  ReadOnlyReal8_ref(transmissivity, static_cast<const MAP_REAL8*>(in[2]));
  ReadOnlyInt4_ref(flowCondition, static_cast<const MAP_INT4*>(in[3]));
  ReadOnlyReal8_ref(storageCoefficient, static_cast<const MAP_REAL8*>(in[4]));
  ReadOnlyReal8_ref(intervalInterface, static_cast<const MAP_REAL8*>(in[5]));
  ReadOnlyReal8_ref(toleranceInterface, static_cast<const MAP_REAL8*>(in[6]));

  PRECOND(!intervalInterface.spatial());
  PRECOND(!toleranceInterface.spatial());

  std::vector<const fieldapi::Common*> inputs;
  inputs.push_back(&elevation);
  inputs.push_back(&recharge);
  inputs.push_back(&transmissivity);
  inputs.push_back(&flowCondition);
  inputs.push_back(&storageCoefficient);
  inputs.push_back(&intervalInterface);
  inputs.push_back(&toleranceInterface);
  PRECOND(inputs.size() == static_cast<size_t>(nrArgs));

  size_t nrRows = elevation.nrRows();
  size_t nrCols = elevation.nrCols();
  size_t r,c;
  PRECOND(nrRows >= 2 && nrCols >= 2);

  // Domain checks...
  std::vector<fieldapi::ScalarDomainCheck> scalarDomains, nsDomains;
  scalarDomains.push_back(fieldapi::ScalarDomainCheck(transmissivity,
                   "transmissivity", com::GreaterThan<double>(0)));
  scalarDomains.push_back(fieldapi::ScalarDomainCheck(storageCoefficient,
                   "storage coefficient", com::GreaterThan<double>(0)));
  nsDomains.push_back(fieldapi::ScalarDomainCheck(intervalInterface,
                   "interval", com::GreaterThan<double>(0)));
  nsDomains.push_back(fieldapi::ScalarDomainCheck(toleranceInterface,
                   "tolerance", com::GreaterThan<double>(0)));

  // Checks...
  int check;

  // Check non spatials.
  check = fieldapi::checkScalarDomains(nsDomains, geo::CellLoc(0, 0));
  if(check != -1) {
    return RetError(1, nsDomains[check].msg().c_str());
  }

  // Check spatials.
  for(geo::CellLocVisitor loc(elevation); loc.valid(); ++loc) {
    check = fieldapi::checkScalarDomains(scalarDomains, *loc);
    if(check != -1) {
      return RetError(1, scalarDomains[check].msg().c_str());
    }
  }

  // Inititalize...
  double oldValue;
  double tolerance = toleranceInterface.value(0, 0);
  double difference, maxDifference;    // Current and max difference.
  size_t nrIterations = 0;

  // yepyep: fieldapi::Common::cellLength();
  MAP_REAL8* map = static_cast<MAP_REAL8*>(out[0]);
  double cellLength = map->CellLength(map);
  PRECOND(cellLength > 0.0);

  // Result elevation is missing value if any of the inputs is MV.
  for(geo::CellLocVisitor visitor(elevation); visitor.valid(); ++visitor) {

    if(fieldapi::nonMV(inputs, *visitor)) {
      resultElevation.copy(elevation, *visitor);
    }
    else {
      resultElevation.putMV(*visitor);
    }
  }

  DiffuseAlgorithm algorithm(resultElevation, elevation, recharge,
                   transmissivity, flowCondition, storageCoefficient,
                   cellLength * cellLength, 0.5, intervalInterface.value(0, 0));
  // std::cout << std::endl;

  // Algorithm...
  // Loop over all cells in the raster untill the maximum difference between
  // the current elevation and the new elevation is smaller than the tolerance.
  do {
    maxDifference = 0.0;

    for( r = 0; r < nrRows; ++r) {
      for( c = 0; c < nrCols; ++c) {

        // Skip missing values.
        if(resultElevation.isMV(geo::CellLoc(r, c))) {
          continue;
        }

        // Remember current elevation.
        oldValue = resultElevation.value(r, c);

        // Select calculation method for current cell, based on flow condition.
        if(flowCondition.value(r, c) == DiffuseAlgorithm::CALCULATE) {
          resultElevation.put(algorithm.calcElevation(r, c), r, c);
        }
        // (else FIXED, NOFLOW)

        // Determine difference.
        difference = ABS(resultElevation.value(r, c) - oldValue);
        if(difference > maxDifference) {
          maxDifference = difference;
        }
      }
    }

    ++nrIterations;

    // yepyep: test op aantal iteraties?
    // PRECOND(nrIterations < 30);
    // std::cout << '.' << std::flush;

  } while (maxDifference > tolerance);

  // std::cout << ": " << nrIterations << std::endl;

  return 0;
}
