#include "stddefx.h"
#include "calc_iocsffieldstrategy.h"
#include "com_exception.h"
#include "com_pathname.h"
#include "dal_FilesystemUtils.h"
#include "geo_csfmap.h"
#include "calc_vs.h"
#include "calc_csfmap.h"
#include "calc_inputspatial.h"
#include "calc_stackinfo.h"
#include "calc_filewriter.h"
#include "calc_stackreader.h"

#include <filesystem>
#include <vector>

/*!
  \file
  This file contains the implementation of the IoCsfFieldStrategy class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOCSFFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

namespace calc {
  typedef StackReaderT<CsfMap> CsfStackReader;

  template <>
  VS CsfStackReader::checkItem(size_t t, VS expectVsSet) const
  {
    VS const vs=expectedFileType(itemName(t),expectVsSet);
    checkClone(t); // pcrcalc/test344(a)

    return vs;
  }

}


//------------------------------------------------------------------------------
// DEFINITION OF IOCSFFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoCsfFieldStrategy::IoCsfFieldStrategy()
{
}

//! dtor
calc::IoCsfFieldStrategy::~IoCsfFieldStrategy()
{
}

//! return PCRaster strategy type
APP_IO_STRATEGY calc::IoCsfFieldStrategy::strategyType() const
{
 return APP_IO_PCRASTER;
}


//! detect correct CSF map
calc::IoFieldStrategy* calc::IoCsfFieldStrategy::checkInputMap(
    VS&   vs,
    const std::string &fName)
{
    // load value as csf map
    vs = expectedFileType(fName,VS_FIELD);
    return this;
}

//! return a newly allocated CSF stack reader
/*!
   callee must delete
 */
const calc::StackReader* calc::IoCsfFieldStrategy::createStackReader(
    const RunDirectory& rd,
    const std::string& stackName)
{
  std::string const stackPrefix(com::PathName(stackName).baseName());

  // expand to existing of first time step
  // exiting because rd must be traversed to find where
  // the stack is located

  com::PathName pn(pathTimestep1(rd,stackName));
  pn.up(); // strip baseName of first time step
  pn += stackPrefix;

  PRECOND(pn.toString().size() > 0);
  return new CsfStackReader(this, pn.toString());
}

//! check against clone, if not clone not yet set, set clone to mapFileName
/*! precondition: \a mapFileName is known to be an existing csf map
 */
void calc::IoCsfFieldStrategy::checkClone(const std::string& mapFileName)
{
    geo::CSFMap const map(mapFileName);
    geo::RasterSpace const mapRs(map.rasterSpace());

    setAndCheckCommon(mapFileName,mapRs);

    if (!d_rasterSpaceCsf.nrRows()) { // not yet initialized
        d_cloneNameCsf = mapFileName;
        d_rasterSpaceCsf = mapRs;
    }

    if (!(mapRs == d_rasterSpaceCsf))
        throwCloneDiffers(d_cloneNameCsf,mapFileName);
}

//! return a newly created CsfMap
calc::GridMap *calc::IoCsfFieldStrategy::createMap(
    const std::string& fileName, VS vs) const
{
  return new CsfMap(fileName, rasterSpace(), vs);
}

//! return prefix-ItemNumber name
std::string calc::IoCsfFieldStrategy::makeStackItemName(
    const std::string& iname,
    int   atTimeStep) const
{
  return dal::timeStepPath83(
      std::filesystem::path(iname),
      atTimeStep).string();
}

//! adjust min max for every map part of the map stack
void calc::IoCsfFieldStrategy::setStackInfo(const StackInfo& s) const
{
  if (!s.d_minMaxSet)
    return;   // all mv!
  // otherwise adjust min and max
  for(size_t t=1; t <= s.d_nrTimeSteps; t++)
   if (s.d_fileWriter->writeThisTimeStep(t))
     try {
      geo::CSFMap m(s.d_fileWriter->mapFileName(t), true);
      m.setMinMax(s.d_min, s.d_max);
     } catch (const com::OpenFileError& /* e */) {
        // ignore if file has disapeared
     }
}

//! return new InputCsfMap object
calc::Spatial *calc::IoCsfFieldStrategy::newInputMap(const std::string& mapName,VS vs,
    const Compressor& c) const
{
  return new InputSpatial<CsfMap>(mapName,vs,c);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
