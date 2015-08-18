#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif
#ifndef INCLUDED_DAL_CSFRASTERDRIVER
#include "dal_CSFRasterDriver.h"
#define INCLUDED_DAL_CSFRASTERDRIVER
#endif
#ifndef INCLUDED_DAL_GDALRASTERDRIVER
#include "dal_GDALRasterDriver.h"
#define INCLUDED_DAL_GDALRASTERDRIVER
#endif
#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif
#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif




// Module headers.
#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif
#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif
#ifndef INCLUDED_CALC_STACKINFO
#include "calc_stackinfo.h"
#define INCLUDED_CALC_STACKINFO
#endif

/*!
  \file
  This file contains the implementation of the IoFieldStrategy class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF IOFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoFieldStrategy::IoFieldStrategy(APP_IO_STRATEGY strategyType)
{
 typedef boost::shared_ptr<dal::RasterDriver> S;
 switch(strategyType) {
   case APP_IO_ESRIGRID:
      POSTCOND(false);
      break;
   case APP_IO_PCRASTER:
      d_outDriver= S(new dal::CSFRasterDriver());
      break;
   case APP_IO_BANDMAP:
      d_outDriver= S(new dal::GDALRasterDriver("EHdr"));
      break;
 }
}

//! dtor
calc::IoFieldStrategy::~IoFieldStrategy()
{
}

//! Copy constructor.
calc::IoFieldStrategy::IoFieldStrategy(const IoFieldStrategy& rhs):
  d_outDriver(rhs.d_outDriver),
  d_commonRS(rhs.d_commonRS),
  d_cloneNameCommon(rhs.d_cloneNameCommon),
  d_cloneNameCsf(rhs.d_cloneNameCsf),
  d_rasterSpaceCsf(rhs.d_rasterSpaceCsf)
#ifdef DEBUG_DEVELOP
  ,d_readFiles(rhs.d_readFiles)
#endif
{
}

void calc::IoFieldStrategy::setRasterSpace(const geo::RasterSpace& rs)
{
  d_commonRS=rs;
  // not set by file checking:
  d_cloneNameCommon.clear();
}

//! compare different formats on their common features
void calc::IoFieldStrategy::checkCommonCloneEqual(
    const std::string& mapFileName,
    const geo::RasterSpace& newMap)  const
{
    if (d_commonRS.nrRows()   != newMap.nrRows() ||
        d_commonRS.nrCols()   != newMap.nrCols() ||
        d_commonRS.cellSize() != newMap.cellSize())
         throwCloneDiffers(d_cloneNameCommon,mapFileName);
}

//! throw com::Exception, if stuff differs
void calc::IoFieldStrategy::throwCloneDiffers(
    const std::string& map1,
    const std::string& map2) const
{
    throw com::Exception("location attributes of "+ quote(map1)+
                " and "+quote(map2)+" are different");
}

//! Remove an output object to ensure proper re-creation.
/*! This method ensures that an object with name \a objName can
    can be created after calling this methode.
    In normal Csf output it does not do anything,
    since the map and tss creation routines will simply create single
    files, destroying old ones under that name. 
    \todo
      check for read-only-ness here?
 */
void calc::IoFieldStrategy::removeOutputObject(const std::string& ) const
{
}

std::string calc::IoFieldStrategy::pathTimeStep1(
    const RunDirectory& rd,
    const std::string&  stackName) const
{
  bool found;
  return rd.inPath(found,makeStackItemName(stackName,1));
}

//! set and check against common clone
/*! A map named mapFileName and loc. attributes mapRs
    is set as common clone, if no clone yet set.
    The same map is checked against a possible already set clone.
 */
void calc::IoFieldStrategy::setAndCheckCommon(
    const std::string& mapFileName,
    const geo::RasterSpace& mapRs)
{
    if (!d_commonRS.valid()) { // not yet initialized
        d_cloneNameCommon = mapFileName;
        d_commonRS        = mapRs;
    }
    checkCommonCloneEqual(mapFileName,mapRs); // pcrcalc/test289
}

//! return raster space location attributes
const geo::RasterSpace& calc::IoFieldStrategy::rasterSpace() const
{
    return d_commonRS;
}

//! overwrite if one needs a format specific filename validator
void calc::IoFieldStrategy::validateFileName(const std::string& ) const
{
}

calc::GridStat calc::IoFieldStrategy::writeFieldUnpacked(
        const std::string& fileName,
                const Field *f) const
{
    GridMapOut m(fileName,*d_outDriver,rasterSpace(), f->vs());
    if (f->isSpatial())
      return m.writeSpatial(f->src());
    return m.writeNonSpatial(f->src());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! detect correct map and update/sync clone
void calc::IoFieldStrategy::checkInputMap(
    VS&   vs,
    const std::string &fName)
{

  // test if is an existing file
  com::testOpenForReading(fName);

  GridMapIn mapFile(fName);
  vs = mapFile.vs();

  checkClone(fName);
}

std::string calc::IoFieldStrategy::inPathStack(
    const RunDirectory& rd,
    const std::string& stackName,
    size_t /* nrTimeSteps */)
{
  std::string stackPrefix(com::PathName(stackName).baseName());

  // expand to existing of first time step
  // must be existing because rd must be traversed to find where
  // the stack is located

  com::PathName pn(pathTimeStep1(rd,stackName));
  pn.up(); // strip baseName of first time step
  pn += stackPrefix;
  pn.makeNative();

  PRECOND(pn.toString().size() > 0);
  return pn.toString();
}

//! check against clone, if not clone not yet set, set clone to mapFileName
/*! precondition: \a mapFileName is known to be an existing csf map
 */
void calc::IoFieldStrategy::checkClone(const std::string& mapFileName)
{
    GridMapIn          map(mapFileName);
    geo::RasterSpace mapRs(map.rasterSpace());

    // check some
    setAndCheckCommon(mapFileName,mapRs);

    if (!d_rasterSpaceCsf.valid()) { // not yet initialized
        d_cloneNameCsf = mapFileName;
        d_rasterSpaceCsf = mapRs;
    }
    // check more specific
    if (!(mapRs == d_rasterSpaceCsf))
        throwCloneDiffers(d_cloneNameCsf,mapFileName);
}


//! return prefix-ItemNumber name
//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      KDJ: There is a variant which takes a string and creates the
             resulting native path for you. See also dal::pathFor which also
             checks whether the iname is a valid native name.
*/
std::string calc::IoFieldStrategy::makeStackItemName(
    const std::string& iname,
    int   atTimeStep) const
{
  boost::filesystem::path p =dal::timeStepPath83(boost::filesystem::path(iname),atTimeStep);
  com::PathName pn(p.string());
  pn.makeNative();
  return pn.toString();
}

//! adjust min max for every map part of the map stack
void calc::IoFieldStrategy::setStackInfo(const StackInfo& s) const
{
  if (!s.d_minMaxSet)
    return;   // all mv!
  for(size_t t=s.startInt(); t <= s.lastInt(); t++)
    if (s.reportTimeStep(t))
     try {
      // TODO no support for other formats!
      geo::CSFMap m(makeStackItemName(s.stackName(),t), true);
      m.setMinMax(s.d_min, s.d_max);
     } catch (const com::Exception& ) {
        // ignore if file has disapeared or other disaster;
     }
}

//! return data into \a dest
void calc::IoFieldStrategy::readField(void *dest, const std::string& mapName,VS vs) const
{
#ifdef DEBUG_DEVELOP
  // check that each item is only read once
  // There is no harm reading more then once except a performance
  // penalty. If TODO's are solved this should always work:
  //  - pcrcalc523 if different input bindings point to the same file!
  //  - pcrcalc524 if different input bindings point to the same file!
  //  - pcrcalc525 if different input bindings point to the same file!
  //  - if areamap is read and same is used as model input
  //  - solved bug with lastUse UseDefAnalyzerTest::nestedLoops()
  if (d_readFiles.count(mapName)) {
    std::string debugDevelopOnly(mapName);
    if (debugDevelopOnly == "inp1b.map") // in known tests to fail
     throw com::OpenFileError(mapName,"IoFieldStrategy::createField");
    // PRINT_VAR(debugDevelopOnly);
  }
  d_readFiles.insert(mapName);
#endif
  GridMapIn m(mapName);
  m.createSpatial(dest,vs);
}
