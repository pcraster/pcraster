#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RUNTIMEENVSETTINGS
#include "calc_runtimeenvsettings.h"
#define INCLUDED_CALC_RUNTIMEENVSETTINGS
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h" // setRan
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif
// Module headers.
#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif

/*!
  \file
  This file contains the implementation of the RunTimeEnvSettings class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNTIMEENVSETTINGS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RUNTIMEENVSETTINGS MEMBERS
//------------------------------------------------------------------------------


calc::RunTimeEnvSettings::RunTimeEnvSettings():
  d_ioStrategy(new IOStrategy()),
  d_externalBindingFile(),
  d_printShellExpansionOnly(false),
  d_testScriptRunableOnly(false),
  d_testCaseTypeOnExistingName(false),
  d_scriptFile(),
  d_compile(false),
  d_exitValueType(ALWAYS_0),
  d_seed(0),  // take current time for seed
  d_profile(false),
  d_useDiskStorage(false)
{
}

calc::RunTimeEnvSettings::~RunTimeEnvSettings()
{
  delete d_ioStrategy;
}

//! Assignment operator.
calc::RunTimeEnvSettings& calc::RunTimeEnvSettings::operator=(const RunTimeEnvSettings& rhs)
{
  if (this != &rhs) {
      delete d_ioStrategy;
      d_ioStrategy                = new IOStrategy(*rhs.d_ioStrategy);
      d_externalBindingFile       = rhs.d_externalBindingFile;
      d_printShellExpansionOnly   = rhs.d_printShellExpansionOnly;
      d_testScriptRunableOnly     = rhs.d_testScriptRunableOnly;
      d_testCaseTypeOnExistingName= rhs.d_testCaseTypeOnExistingName;
      d_scriptFile                = rhs.d_scriptFile;
      d_compile                   = rhs.d_compile;
      d_exitValueType             = rhs.d_exitValueType;
      d_seed                      = rhs.d_seed;
      d_profile                   = rhs.d_profile;
      d_useDiskStorage            = rhs.d_useDiskStorage;
  }
  return *this;
}

//! Copy constructor.
calc::RunTimeEnvSettings::RunTimeEnvSettings(const RunTimeEnvSettings& rhs):
      d_ioStrategy                ( new IOStrategy(*rhs.d_ioStrategy)),
      d_externalBindingFile       ( rhs.d_externalBindingFile),
      d_printShellExpansionOnly   ( rhs.d_printShellExpansionOnly),
      d_testScriptRunableOnly     ( rhs.d_testScriptRunableOnly),
      d_testCaseTypeOnExistingName( rhs.d_testCaseTypeOnExistingName),
      d_scriptFile                ( rhs.d_scriptFile),
      d_compile                   ( rhs.d_compile),
      d_exitValueType             ( rhs.d_exitValueType),
      d_seed                      ( rhs.d_seed),
      d_profile                   ( rhs.d_profile),
      d_useDiskStorage            ( rhs.d_useDiskStorage)
{
}

//! set value of mvCompression
void calc::RunTimeEnvSettings::setMVCompression(bool mvCompression)
{
  d_ioStrategy->setMVCompression(mvCompression);
}

//! set value of debugMVAssignments
void calc::RunTimeEnvSettings::setDebugMVAssignments(const std::string& debugMVAssignmentsMap)
{
  d_ioStrategy->setDebugMVAssignmentsMap(debugMVAssignmentsMap);
}

//! set value of writeEachTimeStep
void calc::RunTimeEnvSettings::setWriteEachTimeStep(bool writeEachTimeStep)
{
  d_ioStrategy->setWriteEachTimeStep(writeEachTimeStep);
}

void calc::RunTimeEnvSettings::setRunDirectory(const com::PathName& runDirectory)
{
  d_ioStrategy->setRunDirectory(runDirectory);
}

//! set value of exitValueType
void calc::RunTimeEnvSettings::setExitValueType(ExitValueType exitValueType)
{
  d_exitValueType=exitValueType;
}

//! set value of seed
void calc::RunTimeEnvSettings::setSeed(size_t seed)
{
  d_seed=seed;
  setRan(seed);
}

//! get value of exitValueType
calc::RunTimeEnvSettings::ExitValueType calc::RunTimeEnvSettings::exitValueType() const
{
  return d_exitValueType;
}

//! get value of seed
size_t calc::RunTimeEnvSettings::seed() const
{
  return d_seed;
}

//! get value of ioStrategy
const calc::IOStrategy& calc::RunTimeEnvSettings::ioStrategy() const
{
  return *d_ioStrategy;
}


//! set value of d_externalBindingFile
void calc::RunTimeEnvSettings::setExternalBindingFile(const com::PathName& externalBindingFile)
{
  d_externalBindingFile=externalBindingFile;
}

//! set value of d_printShellExpansionOnly
void calc::RunTimeEnvSettings::setPrintShellExpansionOnly(bool printShellExpansionOnly)
{
  d_printShellExpansionOnly=printShellExpansionOnly;
}

//! set value of d_testScriptRunableOnly
void calc::RunTimeEnvSettings::setTestScriptRunableOnly(bool testScriptRunableOnly)
{
  d_testScriptRunableOnly=testScriptRunableOnly;
}

//! set value of d_testCaseTypeOnExistingName
void calc::RunTimeEnvSettings::setTestCaseTypeOnExistingName(bool testCaseTypeOnExistingName)
{
  d_testCaseTypeOnExistingName=testCaseTypeOnExistingName;
}

//! set value of d_scriptFile
void calc::RunTimeEnvSettings::setScriptFile(const com::PathName& scriptFile)
{
  d_scriptFile=scriptFile;
}

//! get value of d_externalBindingFile
const com::PathName& calc::RunTimeEnvSettings::externalBindingFile() const
{
  return d_externalBindingFile;
}

//! get value of d_printShellExpansionOnly
bool calc::RunTimeEnvSettings::printShellExpansionOnly() const
{
  return d_printShellExpansionOnly;
}

//! get value of d_testScriptRunableOnly
bool calc::RunTimeEnvSettings::testScriptRunableOnly() const
{
  return d_testScriptRunableOnly;
}

//! get value of d_testCaseTypeOnExistingName
bool calc::RunTimeEnvSettings::testCaseTypeOnExistingName() const
{
  return d_testCaseTypeOnExistingName;
}

//! get value of d_scriptFile
const com::PathName& calc::RunTimeEnvSettings::scriptFile() const
{
  return d_scriptFile;
}


//! set value of d_compile
void calc::RunTimeEnvSettings::setCompile(bool compile)
{
  d_compile=compile;
}

//! get value of d_compile
bool calc::RunTimeEnvSettings::compile() const
{
  return d_compile;
}

//! set value of d_profile
void calc::RunTimeEnvSettings::setProfile(bool profile)
{
  d_profile=profile;
}

//! get value of d_profile
bool calc::RunTimeEnvSettings::profile() const
{
  return d_profile;
}

//! set value of d_useDiskStorage
void calc::RunTimeEnvSettings::setUseDiskStorage(bool useDiskStorage)
{
  d_useDiskStorage=useDiskStorage;
}

//! get value of d_useDiskStorage
bool calc::RunTimeEnvSettings::useDiskStorage() const
{
  return d_useDiskStorage;
}



const geo::RasterSpace& calc::RunTimeEnvSettings::rasterSpace() const
{
  return d_ioStrategy->rasterSpace();
}

//! update symbols, check settings
/*!
 * perform all resolve actions needed before actual executing:
 *  - is all input data available?
 *  - update the types of the data in \a symbols
 *  - check consistency of types and data
 */
void  calc::RunTimeEnvSettings::resolve(
    ASTSymbolTable& symbols,
    std::string const& areaMap,
    const Timer& timer)
{
  d_ioStrategy->resolve(symbols,areaMap,timer);
}

/*! set settings from the XML areaMap and executionOptions sections
 *  \returns the area map reference (binding to definition) if found, empty otherwise
 */
std::string  calc::RunTimeEnvSettings::setSettingsFromXML(
    pcrxml::Script const& s)
{
  if (s.executionOptions().present()) {
     pcrxml::ExecutionOptions const& e(s.executionOptions().get());

     if (e.outputMapFormat().present()) {
      //  set outputMapFormat
      pcrxml::OutputMapFormat const& omf(e.outputMapFormat().get());
      // if (omf.pcrasterMapFormat())
      //  ; // default
      if (omf.bandMapFormat()) {
         appIOstrategy=APP_IO_BANDMAP;
      }
      if (omf.esriGridFormat()) {
         appIOstrategy=APP_IO_ESRIGRID;
      }
    }

    if (e.diagonal().present()) {
       appDiagonal= e.diagonal().get();
    }
    if (e.twoColumnTableAsMatrix().present()) {
       app2dMatrix= e.twoColumnTableAsMatrix().get();
    }
    if (e.keepEdgePits().present()) {
       appPitOnBorder= e.keepEdgePits().get();
    }
    if (e.trueCellUnits().present()) {
       appUnitTrue= e.trueCellUnits().get();
    }
    if (e.cellCoordinate().present()) {
      if (e.cellCoordinate().get()== pcrxml::CellCoordinate::Centre)
        appCoord=APP_C;
      if (e.cellCoordinate().get()== pcrxml::CellCoordinate::LowerRight)
        appCoord=APP_LR;
      if (e.cellCoordinate().get()== pcrxml::CellCoordinate::UpperLeft)
        appCoord=APP_UL;
    }
    if (e.directionalValueUnit().present()) {
      if (e.directionalValueUnit().get()== pcrxml::DirectionalValueUnit::Radians)
        appDirection=APP_RADIANS;
      if (e.directionalValueUnit().get()== pcrxml::DirectionalValueUnit::Degrees)
        appDirection=APP_DEGREES;
    }
    if (e.lddCreateDemMethod().present()) {
      if (e.lddCreateDemMethod().get()== pcrxml::LddCreateDemMethod::Cut)
        appLddDemModifier=APP_LDDDEMCUT;
      if (e.lddCreateDemMethod().get()== pcrxml::LddCreateDemMethod::Fill)
        appLddDemModifier=APP_LDDDEMFILL;
    }
    if (e.dynamicWaveRoughness().present()) {
      if (e.dynamicWaveRoughness().get()== pcrxml::DynamicWaveRoughness::Chezy)
        appDynamicWaveRoughness=APP_DWR_CHEZY;
      if (e.dynamicWaveRoughness().get()== pcrxml::DynamicWaveRoughness::Manning)
        appDynamicWaveRoughness=APP_DWR_MANNING;
    }
    if (e.runDirectory().present()) {
       setRunDirectory(e.runDirectory().get());
    }
    if (e.randomGeneratorSeed().present()) {
       setSeed(e.randomGeneratorSeed().get());
    }
    if (e.maskCompression().present()) {
       setMVCompression(true);
    }
    if (e.useDiskStorage().present()) {
       setUseDiskStorage(true);
    }
  }
  // order of statements important, since
  // computationMask will piggy back on the areaMap created here
  std::string areaMapReference;

  if (s.areaMap()) {
   if (s.areaMap()->fieldReference()) {
      areaMapReference = s.areaMap()->fieldReference().get().ref();
   } else
      d_ioStrategy->setXMLAreaMapScript(s.areaMap().get());
  }
  if (s.computationMask())
    d_ioStrategy->setXMLComputationMask(s.computationMask().get());

  return areaMapReference;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



