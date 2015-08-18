#ifndef INCLUDED_MOC
#include "Moc.h"
#define INCLUDED_MOC
#endif

// External headers.

// Project headers.
#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif

#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Moc class.
*/

namespace {

template<typename T1, typename T2>
void copy(
         calc::Field const* source,
         geo::SimpleRaster<T2>& destination);



template<>
void copy<REAL4, double>(
         calc::Field const* source,
         geo::SimpleRaster<double>& destination)
{
  REAL4 const* sourceArray = source->src_f();
  assert(sourceArray);

  double* destinationArray = destination.cells();
  assert(destinationArray);

  dal::copyCells(sourceArray, sourceArray + destination.nrCells(),
         destinationArray);
}



template<typename T1, typename T2>
void copy(
         geo::SimpleRaster<T1> const& source,
         calc::Field* destination);



template<>
void copy<UINT4, REAL4>(
         geo::SimpleRaster<UINT4> const& source,
         calc::Field* destination)
{
  UINT4 const* sourceArray = source.cells();
  assert(sourceArray);

  REAL4* destinationArray = destination->dest_f();
  assert(destinationArray);

  dal::copyCells(sourceArray, sourceArray + source.nrCells(),
         destinationArray);
}



template<>
void copy<double, REAL4>(
         geo::SimpleRaster<double> const& source,
         calc::Field* destination)
{
  double const* sourceArray = source.cells();
  assert(sourceArray);

  REAL4* destinationArray = destination->dest_f();
  assert(destinationArray);

  dal::copyCells(sourceArray, sourceArray + source.nrCells(),
         destinationArray);
}

} // Anonymous namespace



namespace moc {
namespace python {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MOC MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MOC MEMBERS
//------------------------------------------------------------------------------

Moc::Moc(geo::RasterSpace const& space,
         double timeIncrement,
         UINT4 nrParticles,
         calc::Field const* initialConcentration,
         calc::Field const* effectivePorosity,
         calc::Field const* storageCoefficient)

  : _tracker(space, timeIncrement, nrParticles, initialConcentration->src_f(),
         effectivePorosity->src_f(), storageCoefficient->src_f())

{
}



Moc::~Moc()
{
}



boost::python::tuple Moc::transport(
         calc::Field const* fluxField,
         calc::Field const* xVelocityField,
         calc::Field const* yVelocityField,
         calc::Field const* longitudinalDispersionCoefficientField,
         calc::Field const* transverseDispersionCoefficientField,
         calc::Field const* hydraulicHeadField,
         calc::Field const* saturatedThicknessField)
{
  // TODO Test Field's data and value type.

  // // Configure input args.
  // signature.d_input.resize(7);

  // // Flux.
  // signature.d_input[0].st = PCR_ST_BOTH;
  // signature.d_input[0].vs = PCR_VS_S;
  // signature.d_input[0].cr = PCR_CR_REAL8;

  // // Flow x direction.
  // signature.d_input[1].st = PCR_ST_BOTH;
  // signature.d_input[1].vs = PCR_VS_S;
  // signature.d_input[1].cr = PCR_CR_REAL8;

  // // Flow y direction.
  // signature.d_input[2].st = PCR_ST_BOTH;
  // signature.d_input[2].vs = PCR_VS_S;
  // signature.d_input[2].cr = PCR_CR_REAL8;

  // // Longitudinal dispersion coefficient.
  // signature.d_input[3].st = PCR_ST_BOTH;
  // signature.d_input[3].vs = PCR_VS_S;
  // signature.d_input[3].cr = PCR_CR_REAL8;

  // // Transverse dispersion coefficient.
  // signature.d_input[4].st = PCR_ST_BOTH;
  // signature.d_input[4].vs = PCR_VS_S;
  // signature.d_input[4].cr = PCR_CR_REAL8;

  // // Hydraulic head.
  // signature.d_input[5].st = PCR_ST_BOTH;
  // signature.d_input[5].vs = PCR_VS_S;
  // signature.d_input[5].cr = PCR_CR_REAL8;

  // // Saturated thickness.
  // signature.d_input[6].st = PCR_ST_BOTH;
  // signature.d_input[6].vs = PCR_VS_S;
  // signature.d_input[6].cr = PCR_CR_REAL8;

  // ---------------------------------------------------------------------------
  // // Configure output.
  // signature.d_result.resize(2);

  // // Solute concentration.
  // signature.d_result[0].st = PCR_ST_SPATIAL;
  // signature.d_result[0].vs = PCR_VS_S;
  // signature.d_result[0].cr = PCR_CR_REAL8;

  // // Number of particles per cell.
  // signature.d_result[1].st = PCR_ST_SPATIAL;
  // signature.d_result[1].vs = PCR_VS_S;
  // signature.d_result[1].cr = PCR_CR_REAL8;

  size_t const nrRows = _tracker.nrRows();
  size_t const nrCols = _tracker.nrCols();
  size_t const nrCells = nrRows * nrCols;

  geo::SimpleRaster<double> flux(nrRows, nrCols);
  geo::SimpleRaster<double> xVelocity(nrRows, nrCols);
  geo::SimpleRaster<double> yVelocity(nrRows, nrCols);
  geo::SimpleRaster<double> longitudinalDispersionCoefficient(nrRows, nrCols);
  geo::SimpleRaster<double> transverseDispersionCoefficient(nrRows, nrCols);
  geo::SimpleRaster<double> hydraulicHead(nrRows, nrCols);
  geo::SimpleRaster<double> saturatedThickness(nrRows, nrCols);

  copy<REAL4, double>(fluxField, flux);
  copy<REAL4, double>(xVelocityField, xVelocity);
  copy<REAL4, double>(yVelocityField, yVelocity);
  copy<REAL4, double>(longitudinalDispersionCoefficientField,
         longitudinalDispersionCoefficient);
  copy<REAL4, double>(transverseDispersionCoefficientField,
         transverseDispersionCoefficient);
  copy<REAL4, double>(hydraulicHeadField, hydraulicHead);
  copy<REAL4, double>(saturatedThicknessField, saturatedThickness);

  // Calculate concentrations.
  _tracker.calculateConcentration(flux, xVelocity, yVelocity,
         longitudinalDispersionCoefficient, transverseDispersionCoefficient,
         hydraulicHead, saturatedThickness);

  // Get new concentrations.
  geo::SimpleRaster<REAL8> concentration(nrRows, nrCols);
  _tracker.concentration(concentration);
  boost::shared_ptr<calc::Field> concentrationField(
         new calc::Spatial(VS_S, calc::CRI_f, nrCells));
  copy<double, REAL4>(concentration, concentrationField.get());

  geo::SimpleRaster<UINT4> nrParticlesPerCell(nrRows, nrCols);
  _tracker.nrParticles(nrParticlesPerCell);
  boost::shared_ptr<calc::Field> nrParticlesPerCellField(
         new calc::Spatial(VS_S, calc::CRI_f, nrCells));
  copy<UINT4, REAL4>(nrParticlesPerCell, nrParticlesPerCellField.get());

  return boost::python::make_tuple(concentrationField, nrParticlesPerCellField);
}



boost::shared_ptr<calc::Field> Moc::adjust(
         calc::Field const* changeInConcentration)
{
  // TODO Test Field's data and value type.

  // signature.d_input.resize(1);

  // // Change in concentration: c >= -1.
  // signature.d_input[0].st = PCR_ST_BOTH;
  // signature.d_input[0].vs = PCR_VS_S;
  // signature.d_input[0].cr = PCR_CR_REAL8;

  // signature.d_result.resize(1);

  // // Solute concentration.
  // signature.d_result[0].st = PCR_ST_SPATIAL;
  // signature.d_result[0].vs = PCR_VS_S;
  // signature.d_result[0].cr = PCR_CR_REAL8;

  size_t const nrRows = _tracker.nrRows();
  size_t const nrCols = _tracker.nrCols();
  size_t const nrCells = nrRows * nrCols;

  // Copy change in concentration from field.
  geo::SimpleRaster<double> raster(nrRows, nrCols);
  copy<REAL4, double>(changeInConcentration, raster);

  // Check domain.
  com::GreaterThanEqualTo<double> interval(-1.0);

  for(geo::CellLocVisitor loc(nrRows, nrCols); loc.valid(); ++loc) {
    if(!pcr::isMV(raster.cell(*loc)) && !interval.valid(raster.cell(*loc))) {
      std::string message = "Change in concentration must be " + interval.msg();
      throw com::Exception(message);
    }
  }

  // Adjust concentrations.
  _tracker.adjustConcentration(raster);

  // Get new concentrations.
  geo::SimpleRaster<REAL8> concentration(nrRows, nrCols);
  _tracker.concentration(concentration);

  // Copy concentrations to field.
  boost::shared_ptr<calc::Field> field(
         new calc::Spatial(VS_S, calc::CRI_f, nrCells));
  copy<double, REAL4>(concentration, field.get());

  return field;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace python
} // namespace moc

