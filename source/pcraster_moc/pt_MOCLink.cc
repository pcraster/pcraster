#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PT_MOCLINK
#include "pt_MOCLink.h"
#define INCLUDED_PT_MOCLINK
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

// Module headers.
#ifndef INCLUDED_PT_PARTICLETRACKER
#include "pt_ParticleTracker.h"
#define INCLUDED_PT_PARTICLETRACKER
#endif



/*!
  \file
  This file contains the implementation of the MOCLink class.
*/



//------------------------------------------------------------------------------

/*
namespace pt {

class MOCLinkPrivate
{
private:

  ParticleTracker d_tracker;

public:

  MOCLinkPrivate()
    : d_tracker()
  {
  }

  ~MOCLinkPrivate()
  {
  }

};

} // namespace pt
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MOCLINK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MOCLINK MEMBERS
//------------------------------------------------------------------------------

pt::MOCLink::MOCLink()
  : calc::ModelLink(), d_tracker(0)
{
}



pt::MOCLink::~MOCLink()
{
  if(d_tracker) {
    delete d_tracker;
  }
}



const std::string& pt::MOCLink::name() const
{
  static const std::string name = "moc";
  return name;
}



bool pt::MOCLink::methodCheck(const std::string& methodName,
                   calc::ModelLinkMethodSignature& signature) const
{
  if(methodName == "transport") {
    transportCheck(signature);
    return true;
  }
  else if(methodName == "adjustconcentration") {
    adjustConcentrationCheck(signature);
    return true;
  }

  return false;
}



void pt::MOCLink::methodExecute(const std::string& methodName,
                  calc::ModelLinkMethodSignature& signature)
{
  if(methodName == "transport") {
    transportExecute(signature);
    return;
  }
  else if(methodName == "adjustconcentration") {
    adjustConcentrationExecute(signature);
    return;
  }

  // This only happens if check is implemented, but execute is not.
  throw calc::ModelLinkException("unknown method");
}



void pt::MOCLink::initCheck(calc::ModelLinkMethodSignature& signature) const
{
  // Configure input args.
  signature.d_input.resize(5);

  // Length of time step.
  signature.d_input[0].st = PCR_ST_NONSPATIAL;
  signature.d_input[0].vs = PCR_VS_S;
  signature.d_input[0].cr = PCR_CR_REAL8;

  // Number of particles per cell.
  signature.d_input[1].st = PCR_ST_NONSPATIAL;
  signature.d_input[1].vs = PCR_VS_S;
  signature.d_input[1].cr = PCR_CR_REAL8;

  // Initial concentration. Must be spatial because it is used to determine the
  // extent of the aquifer.
  signature.d_input[2].st = PCR_ST_SPATIAL;
  signature.d_input[2].vs = PCR_VS_S;
  signature.d_input[2].cr = PCR_CR_REAL8;

  // Storage coefficient.
  signature.d_input[3].st = PCR_ST_BOTH;
  signature.d_input[3].vs = PCR_VS_S;
  signature.d_input[3].cr = PCR_CR_REAL8;

  // Effective porosity.
  signature.d_input[4].st = PCR_ST_BOTH;
  signature.d_input[4].vs = PCR_VS_S;
  signature.d_input[4].cr = PCR_CR_REAL8;
}



void pt::MOCLink::initExecute(
  const geo::RasterSpace& rs,
  calc::ModelLinkMethodSignature& signature)
{
  PRECOND(signature.d_input.size() == 5);

  PRECOND(signature.d_input[0].st == PCR_ST_NONSPATIAL);
  PRECOND(signature.d_input[0].vs == PCR_VS_S);
  PRECOND(signature.d_input[0].cr == PCR_CR_REAL8);

  PRECOND(signature.d_input[1].st == PCR_ST_NONSPATIAL);
  PRECOND(signature.d_input[1].vs == PCR_VS_S);
  PRECOND(signature.d_input[1].cr == PCR_CR_REAL8);

  PRECOND(signature.d_input[2].st == PCR_ST_SPATIAL);
  PRECOND(signature.d_input[2].vs == PCR_VS_S);
  PRECOND(signature.d_input[2].cr == PCR_CR_REAL8);

  PRECOND(signature.d_input[3].vs == PCR_VS_S);
  PRECOND(signature.d_input[3].cr == PCR_CR_REAL8);

  PRECOND(signature.d_input[4].vs == PCR_VS_S);
  PRECOND(signature.d_input[4].cr == PCR_CR_REAL8);

  d_rs = rs;

  // Initialize all MOC stuff. Make sure we are ready for transport calls.
  const PCR_MAP_REAL8* timeStepMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[0].value);
  double timeStep;
  timeStepMap->Get(&timeStep, 0, 0, timeStepMap);
  PRECOND(!pcr::isMV(timeStep));

  const PCR_MAP_REAL8* nrParticlesMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[1].value);
  double nrParticles;
  nrParticlesMap->Get(&nrParticles, 0, 0, nrParticlesMap);
  PRECOND(!pcr::isMV(nrParticles));
  com::GreaterThan<double> nrParticleValidator(0.0);
  if(!nrParticleValidator.valid(nrParticles)) {
    std::string message = "Number of particles per cell must be " +
         nrParticleValidator.msg();
    throw com::Exception(message);
  }

  // Initial concentration.
  const PCR_MAP_REAL8* iniConcMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[2].value);
  geo::SimpleRaster<double> iniConc(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    iniConcMap->Get(&iniConc.cell(*loc), loc.row(), loc.col(), iniConcMap);
  }

  // Storage coefficient.
  const PCR_MAP_REAL8* storageCoefMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[3].value);
  geo::SimpleRaster<double> storageCoef(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    storageCoefMap->Get(&storageCoef.cell(*loc), loc.row(), loc.col(),
         storageCoefMap);
  }

  // Effective porosity.
  const PCR_MAP_REAL8* effPorosityMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[4].value);
  geo::SimpleRaster<double> effPorosity(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    effPorosityMap->Get(&effPorosity.cell(*loc), loc.row(), loc.col(),
         effPorosityMap);
  }

  // Create particle tracker.
  d_tracker = new ParticleTracker(rs, timeStep,
         static_cast<UINT4>(std::floor(nrParticles)), iniConc, storageCoef,
         effPorosity);
}



void pt::MOCLink::transportCheck(calc::ModelLinkMethodSignature& signature) const
{
  // ---------------------------------------------------------------------------
  // Configure input args.
  signature.d_input.resize(7);

  // Flux.
  signature.d_input[0].st = PCR_ST_BOTH;
  signature.d_input[0].vs = PCR_VS_S;
  signature.d_input[0].cr = PCR_CR_REAL8;

  // Flow x direction.
  signature.d_input[1].st = PCR_ST_BOTH;
  signature.d_input[1].vs = PCR_VS_S;
  signature.d_input[1].cr = PCR_CR_REAL8;

  // Flow y direction.
  signature.d_input[2].st = PCR_ST_BOTH;
  signature.d_input[2].vs = PCR_VS_S;
  signature.d_input[2].cr = PCR_CR_REAL8;

  // Longitudinal dispersion coefficient.
  signature.d_input[3].st = PCR_ST_BOTH;
  signature.d_input[3].vs = PCR_VS_S;
  signature.d_input[3].cr = PCR_CR_REAL8;

  // Transverse dispersion coefficient.
  signature.d_input[4].st = PCR_ST_BOTH;
  signature.d_input[4].vs = PCR_VS_S;
  signature.d_input[4].cr = PCR_CR_REAL8;

  // Hydraulic head.
  signature.d_input[5].st = PCR_ST_BOTH;
  signature.d_input[5].vs = PCR_VS_S;
  signature.d_input[5].cr = PCR_CR_REAL8;

  // Saturated thickness.
  signature.d_input[6].st = PCR_ST_BOTH;
  signature.d_input[6].vs = PCR_VS_S;
  signature.d_input[6].cr = PCR_CR_REAL8;

  // ---------------------------------------------------------------------------
  // Configure output.
  signature.d_result.resize(2);

  // Solute concentration.
  signature.d_result[0].st = PCR_ST_SPATIAL;
  signature.d_result[0].vs = PCR_VS_S;
  signature.d_result[0].cr = PCR_CR_REAL8;

  // Number of particles per cell.
  signature.d_result[1].st = PCR_ST_SPATIAL;
  signature.d_result[1].vs = PCR_VS_S;
  signature.d_result[1].cr = PCR_CR_REAL8;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void pt::MOCLink::transportExecute(calc::ModelLinkMethodSignature& signature) const
{
  PRECOND(signature.d_input.size() == 7);
  PRECOND(signature.d_input[0].cr == PCR_CR_REAL8);
  PRECOND(signature.d_input[1].cr == PCR_CR_REAL8);
  PRECOND(signature.d_input[2].cr == PCR_CR_REAL8);
  PRECOND(signature.d_input[3].cr == PCR_CR_REAL8);
  PRECOND(signature.d_input[4].cr == PCR_CR_REAL8);
  PRECOND(signature.d_input[5].cr == PCR_CR_REAL8);
  PRECOND(signature.d_input[6].cr == PCR_CR_REAL8);

  PRECOND(signature.d_result.size() == 2);
  PRECOND(signature.d_result[0].cr == PCR_CR_REAL8);
  PRECOND(signature.d_result[1].cr == PCR_CR_REAL8);

  const PCR_MAP_REAL8* fluxMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[0].value);
  const PCR_MAP_REAL8* velocityVector[2] = {
         static_cast<PCR_MAP_REAL8*>(signature.d_input[1].value),
         static_cast<PCR_MAP_REAL8*>(signature.d_input[2].value) };
  const PCR_MAP_REAL8* dispCoef[2] = {
         static_cast<PCR_MAP_REAL8*>(signature.d_input[3].value),
         static_cast<PCR_MAP_REAL8*>(signature.d_input[4].value) };
  const PCR_MAP_REAL8* headMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[5].value);
  const PCR_MAP_REAL8* satThicknessMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[6].value);

  geo::SimpleRaster<double> flux(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    fluxMap->Get(&flux.cell(*loc), loc.row(), loc.col(), fluxMap);
  }

  geo::SimpleRaster<double> xVeloc(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    velocityVector[0]->Get(&xVeloc.cell(*loc), loc.row(), loc.col(),
         velocityVector[0]);
  }

  geo::SimpleRaster<double> yVeloc(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    velocityVector[1]->Get(&yVeloc.cell(*loc), loc.row(), loc.col(),
         velocityVector[1]);
  }

  geo::SimpleRaster<double> longDisp(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    dispCoef[0]->Get(&longDisp.cell(*loc), loc.row(), loc.col(),
         dispCoef[0]);
  }

  geo::SimpleRaster<double> transDisp(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    dispCoef[1]->Get(&transDisp.cell(*loc), loc.row(), loc.col(),
         dispCoef[1]);
  }

  geo::SimpleRaster<double> head(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    // TODO KDJ: This should be the headMap, shouldn't it???
    satThicknessMap->Get(&head.cell(*loc), loc.row(), loc.col(), headMap);
  }

  geo::SimpleRaster<double> satThickness(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    satThicknessMap->Get(&satThickness.cell(*loc), loc.row(), loc.col(),
         satThicknessMap);
  }

  // Calculate concentrations.
  d_tracker->calculateConcentration(flux, xVeloc, yVeloc, longDisp,
         transDisp, head, satThickness);

  // Get concentrations from particle tracker.
  PCR_MAP_REAL8* c = static_cast<PCR_MAP_REAL8*>(signature.d_result[0].value);
  geo::SimpleRaster<REAL8> conc(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    c->Get(&conc.cell(*loc), loc.row(), loc.col(), c);
  }

  d_tracker->concentration(conc);

  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    if(!pcr::isMV(conc.cell(*loc))) {
      c->Put(static_cast<REAL8>(conc.cell(*loc)), loc.row(), loc.col(), c);
    }
    else {
      c->PutMV(loc.row(), loc.col(), c);
    }
  }

  // Get number of particles per cell from particle tracker.
  PCR_MAP_REAL8* p = static_cast<PCR_MAP_REAL8*>(signature.d_result[1].value);
  geo::SimpleRaster<UINT4> nrParticles(d_rs.nrRows(), d_rs.nrCols());

  d_tracker->nrParticles(nrParticles);

  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    if(!pcr::isMV(nrParticles.cell(*loc))) {
      p->Put(static_cast<REAL8>(nrParticles.cell(*loc)), loc.row(), loc.col(),
         p);
    }
    else {
      p->PutMV(loc.row(), loc.col(), p);
    }
  }
}



void pt::MOCLink::adjustConcentrationCheck(
         calc::ModelLinkMethodSignature& signature) const
{
  signature.d_input.resize(1);

  // Change in concentration: c >= -1.
  signature.d_input[0].st = PCR_ST_BOTH;
  signature.d_input[0].vs = PCR_VS_S;
  signature.d_input[0].cr = PCR_CR_REAL8;

  signature.d_result.resize(1);

  // Solute concentration.
  signature.d_result[0].st = PCR_ST_SPATIAL;
  signature.d_result[0].vs = PCR_VS_S;
  signature.d_result[0].cr = PCR_CR_REAL8;
}



void pt::MOCLink::adjustConcentrationExecute(
         calc::ModelLinkMethodSignature& signature) const
{
  PRECOND(signature.d_input.size() == 1);
  PRECOND(signature.d_input[0].cr == PCR_CR_REAL8);

  PRECOND(signature.d_result.size() == 1);
  PRECOND(signature.d_result[0].cr == PCR_CR_REAL8);

  const PCR_MAP_REAL8* deltaConcMap =
         static_cast<PCR_MAP_REAL8*>(signature.d_input[0].value);

  geo::SimpleRaster<double> deltaConc(d_rs.nrRows(), d_rs.nrCols());
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    deltaConcMap->Get(&deltaConc.cell(*loc), loc.row(),
         loc.col(), deltaConcMap);
  }

  // Domain checks...
  com::GreaterThanEqualTo<double> interval(-1.0);
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    if(!pcr::isMV(deltaConc.cell(*loc)) &&
         !interval.valid(deltaConc.cell(*loc))) {
      std::string message = "Change in concentration must be " + interval.msg();
      throw com::Exception(message);
    }
  }

  d_tracker->adjustConcentration(deltaConc);

  // Get concentrations from particle tracker.
  PCR_MAP_REAL8* c = static_cast<PCR_MAP_REAL8*>(signature.d_result[0].value);
  geo::SimpleRaster<REAL8> conc(d_rs.nrRows(), d_rs.nrCols());

  // TODO KDJ: What is this loop for? It copies values from a c to conc. But
  //           c doesn't contain anything interesting yet, and conc is updated
  //           by the tracker...
  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    c->Get(&conc.cell(*loc), loc.row(), loc.col(), c);
  }

  d_tracker->concentration(conc);

  for(geo::CellLocVisitor loc(d_rs.nrRows(), d_rs.nrCols()); loc.valid();
         ++loc) {
    if(!pcr::isMV(conc.cell(*loc))) {
      c->Put(static_cast<REAL8>(conc.cell(*loc)), loc.row(), loc.col(), c);
    }
    else {
      c->PutMV(loc.row(), loc.col(), c);
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



