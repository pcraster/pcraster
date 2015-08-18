#ifndef INCLUDED_PT_PARTICLETRACKER
#define INCLUDED_PT_PARTICLETRACKER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_GRIDDEDPOINTS
#include "geo_griddedpoints.h"
#define INCLUDED_GEO_GRIDDEDPOINTS
#endif

#ifndef INCLUDED_GEO_POINTVALUE
#include "geo_pointvalue.h"
#define INCLUDED_GEO_POINTVALUE
#endif

#ifndef INCLUDED_GEO_RASTERBOUNDARIES
#include "geo_rasterboundaries.h"
#define INCLUDED_GEO_RASTERBOUNDARIES
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.
#ifndef INCLUDED_PT_PARTICLE
#include "pt_Particle.h"
#define INCLUDED_PT_PARTICLE
#endif



namespace pt {
  // ParticleTracker declarations.
}



namespace pt {



//! This class implements the MOC way to track particles.
/*!
*/
class PCR_DLL_CLASS ParticleTracker
{

private:

  enum Dispersion { DISP_1, DISP_2, DISP_3, DISP_4};

  //! Raster with particles.
  geo::GriddedPoints<Particle> _particles;

  //! Length of time step.
  double           _timeIncrement;

  //! Number of particles per cell.
  UINT4            _nrParticles;

  geo::SimpleRaster<double> _iniConc;

  //! Fraction of the grid dimensions that particles will be allowed to move.
  double           _gamma;

  //! Effective porosity.
  geo::SimpleRaster<double> _effPorosity;

  //! Storage coefficient.
  geo::SimpleRaster<double> _storageCoef;

  //! Velocities in x direction at borders of cells.
  geo::RasterBoundaries<double> _xBorderVeloc;

  //! Velocities in y direction at borders of cells.
  geo::RasterBoundaries<double> _yBorderVeloc;

  //! Magnitude of velocities.
  geo::RasterBoundaries<double> _velocityMagnitude;

  //! Dispersion equation coefficient.
  geo::SimpleRaster<double> _disp1;

  //! Dispersion equation coefficient.
  geo::SimpleRaster<double> _disp2;

  //! Dispersion equation coefficient.
  geo::SimpleRaster<double> _disp3;

  //! Dispersion equation coefficient.
  geo::SimpleRaster<double> _disp4;

  double           _initialMass;

  void             init                ();

  //! Assignment operator. NOT IMPLEMENTED.
  ParticleTracker& operator=           (const ParticleTracker&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ParticleTracker     (const ParticleTracker&);

  bool             aquiferDetermined   () const;

  void             determineAquifer    (const geo::SimpleRaster<double>& raster);

  void             adjustAquifer       (const geo::SimpleRaster<double>& raster);

  void             checkWithAquifer    (const geo::SimpleRaster<double>& parameter);

  void             syncWithAquifer     (geo::SimpleRaster<double>& raster);

  bool             inAquifer           (const geo::CellLoc& loc) const;

  bool             inAquifer           (size_t row, size_t col) const;

  bool             inAquifer           (double x,
                                        double y) const;

  bool             isSourceOrSinkCell  (
                             const geo::SimpleRaster<double>& flux,
                             size_t row,
                             size_t col) const;

  bool             isSourceOrSinkCell  (
                             const geo::SimpleRaster<double>& flux,
                             const geo::CellLoc& loc) const;

  bool             isSourceCell        (
                             const geo::SimpleRaster<double>& flux,
                             size_t row,
                             size_t col) const;

  bool             isSourceCell        (
                             const geo::SimpleRaster<double>& flux,
                             const geo::CellLoc& loc) const;

  bool             isSinkCell          (
                             const geo::SimpleRaster<double>& flux,
                             size_t row,
                             size_t col) const;

  bool             isSinkCell          (
                             const geo::SimpleRaster<double>& flux,
                             const geo::CellLoc& loc) const;

  bool             cellAlongEdgeAquifer(size_t row,
                                        size_t col) const;

  double           timeIncrement       () const;

  double           cellSize            () const;

  double           cellWidth           () const;

  double           cellHeight          () const;

  double           cellArea            () const;

  void             coords2RowCol       (double x,
                                        double y,
                                        double& row,
                                        double& col) const;

  void             coords2RowCol       (double x,
                                        double y,
                                        size_t& row,
                                        size_t& col) const;

  void             rowCol2Coords       (double row,
                                        double col,
                                        double& x,
                                        double& y) const;

  void             loc2Coords          (const geo::CellLoc& loc,
                                        double& x,
                                        double& y) const;

  void             reflect             (size_t row,
                                        size_t col,
                                        double& x,
                                        double& y) const;

  void             generateDistribution(
                             const geo::SimpleRaster<double>& conc);

  void             updateDistribution  (
                             const geo::SimpleRaster<double>& flux);

  void             optimizeConcentrationGradient();

  void             calculateVelocity   (
                             const geo::SimpleRaster<double>& xVeloc,
                             const geo::SimpleRaster<double>& yVeloc);

  void             calculateDispersionEquationCoefficients(
                             const geo::SimpleRaster<double>& longDisp,
                             const geo::SimpleRaster<double>& transDisp,
                             const geo::SimpleRaster<double>& satThickness,
                             geo::RasterBoundaries<double>& Dxx,
                             geo::RasterBoundaries<double>& Dyy);

  size_t           calculateNumberOfParticleMoves(
                             const geo::SimpleRaster<double>& xVeloc,
                             const geo::SimpleRaster<double>& yVeloc,
                             const geo::SimpleRaster<double>& flux,
                             const geo::SimpleRaster<double>& satThickness,
                             const geo::RasterBoundaries<double>& Dxx,
                             const geo::RasterBoundaries<double>& Dyy);

  void             moveParticles       (
                             const geo::SimpleRaster<double>& xVeloc,
                             const geo::SimpleRaster<double>& yVeloc,
                             const geo::SimpleRaster<double>& flux,
                             double timeIncrement);

  double           xVelocity           (
                             const geo::SimpleRaster<double>& xVeloc,
                             const geo::SimpleRaster<double>& flux,
                             size_t row,
                             size_t col,
                             geo::Quadrant quadrant) const;

  double           yVelocity           (
                             const geo::SimpleRaster<double>& yVeloc,
                             const geo::SimpleRaster<double>& flux,
                             size_t row,
                             size_t col,
                             geo::Quadrant quadrant) const;

  void             velocity            (
                             const geo::SimpleRaster<double>& xVeloc,
                             const geo::SimpleRaster<double>& yVeloc,
                             const geo::SimpleRaster<double>& flux,
                             size_t row,
                             size_t col,
                             geo::Quadrant quadrant,
                             double& xVelocParticle,
                             double& yVelocParticle) const;

  double           concentration(
                             const std::vector<geo::PointValue<geo::Point<double, 2>, double> >& points,
                             const geo::CellLoc& loc) const;

  /*
  double           interpolateConcentration(
                             const std::vector<geo::IdiPoint<geo::Point<double, 2> > >& points,
                             const geo::CellLoc& loc) const;
                             */

  double           maxConcentration(
                             const std::vector<geo::PointValue<geo::Point<double, 2>, double> >& points,
                             const geo::CellLoc& loc) const;

  void             averageConcentration(
                             geo::SimpleRaster<double>& conc) const;

  void             sumConcentration(
                             geo::SimpleRaster<double>& conc) const;

  void             changeInConcentrations(
                             const geo::SimpleRaster<double>& conc1,
                             const geo::SimpleRaster<double>& conc2,
                             const geo::SimpleRaster<double>& flux,
                             const geo::SimpleRaster<double>& head,
                             const geo::SimpleRaster<double>& headPrevious,
                             const geo::SimpleRaster<double>& satThickness,
                             const geo::SimpleRaster<double>& satThicknessPrevious,
                             double timeIncrement,
                             geo::SimpleRaster<double>& deltaConc) const;

  double           concentrationGradient(
                             const geo::SimpleRaster<double>& conc,
                             const geo::SimpleRaster<double>& flux,
                             const geo::SimpleRaster<double>& head,
                             const geo::SimpleRaster<double>& headPrevious,
                             const geo::SimpleRaster<double>& satThickness,
                             const geo::SimpleRaster<double>& satThicknessPrevious,
                             double timeIncrement,
                             const geo::CellLoc& loc) const;

  void             adjustConcentration(
                             const geo::SimpleRaster<double>& conc1,
                             const geo::SimpleRaster<double>& conc2,
                             const geo::SimpleRaster<double>& flux,
                             geo::SimpleRaster<double>& deltaConc);

  void             adjustConcentration(
                             const geo::CellLoc& loc,
                             double conc,
                             double deltaConc);

  void             adjustNetMassFlux(
                             const geo::SimpleRaster<double>& conc,
                             const geo::SimpleRaster<double>& flux,
                             double timeIncrement);

  double           mass                (
                             const geo::SimpleRaster<double>& conc,
                             const geo::SimpleRaster<double>& satThickness) const;

  double           mass                (
                             size_t col,
                             const geo::SimpleRaster<double>& conc,
                             const geo::SimpleRaster<double>& satThickness) const;


  double           massGenerated       (
                             double timeIncrement,
                             const geo::SimpleRaster<double>& flux) const;

  double           massLost            (
                             double timeIncrement,
                             const geo::SimpleRaster<double>& flux) const;

  double           netMassFlux         (
                             double timeIncrement,
                             const geo::SimpleRaster<double>& flux) const;

  double           changeInMass        (
                             const geo::SimpleRaster<double>& conc,
                             const geo::SimpleRaster<double>& satThickness) const;

  double           percentageMassError3(
                             double timeIncrement,
                             const geo::SimpleRaster<double>& conc,
                             const geo::SimpleRaster<double>& flux,
                             const geo::SimpleRaster<double>& satThickness) const;

  double           percentageVoidCells () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ParticleTracker     (const geo::RasterSpace& space,
                                        double timeIncrement,
                                        UINT4 nrParticles,
                                        REAL4 const* iniConc,
                                        REAL4 const* effPorosity,
                                        REAL4 const* storageCoef);

                   ParticleTracker     (
                             const geo::RasterSpace& space,
                             double timeIncrement,
                             UINT4 nrParticles,
                             const geo::SimpleRaster<double>& iniConc,
                             const geo::SimpleRaster<double>& effPorosity,
                             const geo::SimpleRaster<double>& storageCoef);

  /* virtual */    ~ParticleTracker    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             calculateConcentration(
                             const geo::SimpleRaster<double>& flux,
                             const geo::SimpleRaster<double>& xVeloc,
                             const geo::SimpleRaster<double>& yVeloc,
                             const geo::SimpleRaster<double>& longDisp,
                             const geo::SimpleRaster<double>& transDisp,
                             const geo::SimpleRaster<double>& head,
                             const geo::SimpleRaster<double>& satThickness);

  void             adjustConcentration (
                             const geo::SimpleRaster<double>& deltaConc);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  geo::RasterSpace const& space        () const;

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  void             concentration       (geo::SimpleRaster<double>& conc) const;

  void             nrParticles         (geo::SimpleRaster<UINT4>& nrParticles);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pt

#endif
