#include "stddefx.h"
#include "calc_ioesrifieldstrategy.h"
#include "com_pathinfo.h"
#include "com_pathname.h"
#include "com_file.h"       // remove
#include "com_directory.h"  // createDirectory
#include "com_strlib.h"
#include "pcrgenxml_directorystackinfo.h"
#include "pcrxml_csfvs2datatype.h"
#include "calc_map2csf.h"  // only for vs -> xml DataType
#include "calc_esrimap.h"
#include "calc_inputspatial.h"
#include "calc_esrigridio.h"
#include "calc_stackreader.h"
#include "calc_iocsffieldstrategy.h"
#include "calc_stackinfo.h"

#include <cstdio>
#include <filesystem>
#include <vector>
#include <stdexcept>

/*!
  \file
  This file contains the implementation of the IoEsriFieldStrategy class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOESRIFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------
namespace calc
{
typedef StackReaderT<EsriMap> EsriStackReader;

template <> VS EsriStackReader::checkItem(size_t t, [[maybe_unused]] VS expectVsSet) const
{
  PRECOND(expectVsSet != VS_UNKNOWN);
  EsriMap const map(itemName(t));

  checkClone(t);

  return map.vs();
}

}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF IOESRIFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
 *  use the CSf strategy as fall back
 */
calc::IoEsriFieldStrategy::IoEsriFieldStrategy()
    : d_esriGrid(new EsriGridIO()), d_fallBack(new IoCsfFieldStrategy())
{
}

//! dtor
calc::IoEsriFieldStrategy::~IoEsriFieldStrategy()
{
  delete d_esriGrid;
  delete d_fallBack;
}

//! return strategy type
APP_IO_STRATEGY calc::IoEsriFieldStrategy::strategyType() const
{
  return APP_IO_ESRIGRID;
}

//! try read an Esri Grid file, if fails check for a csf map
calc::IoFieldStrategy *calc::IoEsriFieldStrategy::checkInputMap(VS &vs, const std::string &fName)
{
  try {
    EsriMap const grid(fName);
    // always prefer an esri grid prj file
    if (grid.prjFile().size()) {
      d_prjFile = grid.prjFile();
    }
    vs = grid.vs();
    return this;
  } catch (NotAnEsriGrid &) {
    // try next format
    d_fallBack->checkInputMap(vs, fName);
    d_fallBack->checkClone(fName);
    if (d_prjFile.empty()) {
      com::PathName prj(fName);
      prj.setExtension("prj");
      if (com::exists(prj)) {
        d_prjFile = prj.toString();
      }
    }
    setAndCheckCommon(fName, d_fallBack->rasterSpace());
    return d_fallBack;
  }
}

//! return a new format specific stack reader
/*! if first timestep is an Esri grid then an Esri reader a
    CSF reader otherwise.
    Callee must delete
 */
const calc::StackReader *calc::IoEsriFieldStrategy::createStackReader(const RunDirectory &rd,
                                                                      const std::string &stackName)
{
  std::string const item1(pathTimestep1(rd, stackName));
  if (EsriMap::exists(item1)) {
    // 1st item is a subdir
    com::PathName pn(item1);
    pn.up();  // strip subdir
    return new EsriStackReader(this, pn.toString());
  }
  // this is tricky, is clone for ESRI correctly set?, see esrigrid/test27
  return d_fallBack->createStackReader(rd, stackName);
}

//! check against clone, if not clone not yet set, set clone to mapFileName
/*! precondition: \a mapFileName is known to be an existing Esri grid
 */
void calc::IoEsriFieldStrategy::checkClone(const std::string &mapFileName)
{
  calc::EsriMap const map(mapFileName);
  geo::RasterSpace const mapRs(map.nrRows(), map.nrCols(), map.cellSize(), 0, 0, geo::YIncrT2B, 0);

  setAndCheckCommon(mapFileName, mapRs);

  if (!d_rasterSpaceEsri.nrRows()) {  // not yet initialized
    d_cloneNameEsri = mapFileName;
    d_rasterSpaceEsri = mapRs;
    map.bbox(d_bbox);
  }

  double bboxMap[4];
  map.bbox(bboxMap);

  if (mapRs.cellSize() != rasterSpace().cellSize() || d_bbox[0] != bboxMap[0] ||
      d_bbox[1] != bboxMap[1] || d_bbox[2] != bboxMap[2] || d_bbox[3] != bboxMap[3]) {
    throwCloneDiffers(d_cloneNameEsri, mapFileName);
  }
}

//! set up the stuff needed for an Esri Grid clone
/*!
    called from calc::IoFieldStrategy::setupClone() as first action
 */
void calc::IoEsriFieldStrategy::setupFormatSpecificClone()
{
  if (!d_rasterSpaceEsri.nrRows()) {
    // Esri output wanted, but no Esri clone detected
    //  ESRI grid always cartesian yb2t
    PRECOND(rasterSpace().nrRows());
    // but garantueed to have a clone (see Script::setupClone())
    // copy one from d_fallBack
    d_rasterSpaceEsri = rasterSpace();
    d_bbox[0] = d_rasterSpaceEsri.left();
    d_bbox[1] = std::min(d_rasterSpaceEsri.bottom(), d_rasterSpaceEsri.top());
    d_bbox[2] = d_rasterSpaceEsri.right();
    d_bbox[3] = std::max(d_rasterSpaceEsri.bottom(), d_rasterSpaceEsri.top());
  }
}

//! return an allocated and created calc::EsriMap
calc::GridMap *calc::IoEsriFieldStrategy::createMap(const std::string &fileName, VS vs) const
{
  com::PathName pn(fileName);
  pn.makeAbsolute();
  // esri-grid needs fileName to be-non-existent
  removeOutputObject(pn.toString());
  auto *m = new EsriMap(pn.toString(), rasterSpace().nrRows(), rasterSpace().nrCols(),
                        rasterSpace().cellSize(), d_bbox, vs);
  if (d_prjFile.size()) {
    m->setPrjFile(d_prjFile);
  }
  return m;
}

/*!
    Checks if it is an esrigrid dir, or map stack and tries
    to remove the contents.
    \exception com::Exception if the name can not be overwritten or deleted
*/
void calc::IoEsriFieldStrategy::removeOutputObject(const std::string &objName) const
{
  com::PathInfo const path(objName);
  if (!path.exists()) {
    return;  // nothing to clean
  }
  if (!path.isDirectory()) {
    // remove file
    remove(path.pathName());
    return;
  }

  // it is a directory

  // can be a single esrigrid
  if (calc::EsriMap::remove(objName)) {
    return;
  }

  // other directory check that

  namespace fs = std::filesystem;
  bool isAStack = true;
  fs::path infoDir;
  fs::path const stack(path.pathName().path());
  fs::directory_iterator const end_iter;
  for (fs::directory_iterator f(stack); f != end_iter && isAStack; ++f) {
    // expect only pcr_esri, info and numeric
    // file names
    if (!com::compareNoCase(f->path().filename().string(), "pcr_esri")) {
      continue;
    }
    if (!com::compareNoCase(f->path().filename().string(), "info")) {
      infoDir = *f;
      continue;
    }
    try {
      (void)com::strToSize_t(f->path().filename().string());
      isAStack = calc::EsriMap::exists(f->path().string());
    } catch (std::range_error &) {
      // a non numeric file name
      isAStack = false;
    }
  }

  if (!isAStack) {  // esrigrid/test17
    throw com::Exception("Can not overwrite " + objName);
  }

  // now it is either a stack or an empty directory
  // both is fine and can be deleted

  for (fs::directory_iterator f(stack); f != end_iter; ++f) {
    if (com::compareNoCase(f->path().filename().string(), "info")) {  // skip info
      removeOutputObject(f->path().string());
    }
  }

  // info directory as last one, remove grid  depends on it
  if (!infoDir.empty()) {
    for (fs::directory_iterator f(infoDir); f != end_iter; ++f) {
      removeOutputObject(f->path().string());
    }
    com::remove(infoDir.string());
  }

  com::remove(path.pathName());
}

//! create directory if not yet existent
static void createDir(const com::PathName &fName)
{
  try {
    com::PathInfo const p(fName);
    if (p.isFile()) {  // esrigrid/test23
      com::remove(fName);
    }
    com::createDirectory(fName);
  } catch (const com::OpenFileError &e) {
    throw std::runtime_error(e.messages());
  }
}

//! return name of stack item at a time step
/*!
   Esri strategy has a directory named stackName
   with each grid named as 08d number.
 */
std::string calc::IoEsriFieldStrategy::makeStackItemName(const std::string &stackName,
                                                         int atTimeStep) const
{
  // directory name of stack
  com::PathName fName(stackName);
  createDir(fName);
  const size_t buf_size = 12;
  char buf[buf_size];
  std::snprintf(buf, buf_size, "%08d", atTimeStep);
  // name of ESRI grid
  fName += buf;
  fName.makeAbsolute();
  return fName.toString();
}

//! writes the pcr_esri min/max file
/*! \todo
       update delphi-DLL for new PCRaster DTD.
    \todo
       do dataType in directoryStackInfo
    \todo
       assure that float read correct in Delphi , or . issue
       punt of komma dus!
 */
void calc::IoEsriFieldStrategy::setStackInfo(const StackInfo &s) const
{
  pcrxml::DirectoryStackInfo dsi;
  dsi.allMissingValue = !s.d_minMaxSet;
  dsi.stackEnd = s.d_nrTimeSteps;
  dsi.minimumValue = s.d_minMaxSet ? s.d_min : 0;
  dsi.maximumValue = s.d_minMaxSet ? s.d_max : 0;
  dsi.dataTypeDTD = pcrxml::csfVs2DataType(vs2CsfVs(s.d_vs));

  com::PathName fName(s.d_stackName);
  createDir(fName);
  fName += "pcr_esri";
  dsi.write(fName);
}

/*! check on ESRI grid restrictions when parameter is
    written as a single grid, not a tss or stack, happens
    only in initial section
 */
void calc::IoEsriFieldStrategy::validateFileName(const std::string &fileName) const
{
  size_t const len = fileName.size();
  // MODELBUILDER FF uit eigenlijk baseName alleen te lang
  if (len >= 14) {  // esrigrid/test5a
    throw com::Exception("ESRI grid name too long, max. is 13 characters");
  }
  // MODELBUIDER
  if (fileName.find('.') < len) {  // esrigrid/test5
    throw com::Exception("ESRI grid name can not contain a .-symbol");
  }
}

//! return new InputEsriMap object
calc::Spatial *calc::IoEsriFieldStrategy::newInputMap(const std::string &mapName, VS vs,
                                                      const Compressor &c) const
{
  return new InputSpatial<EsriMap>(mapName, vs, c);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

#include "pcrdll.h"

#include "pcrxml_document.h"

#include <cstring>  // strcpy

//! read contents of an pcr_esri file
/*!
    read in \a dirName pcr_esri assuming it to be an XML file
     with DOCTYPE DirectoryStackInfo.
    All arguments except \a dirName are return values
    of the identical attribute.

    \param dirName     name of directory that has xml file named pcr_esri
    \param allMissingValue  see above, a boolean as int
    \param minimumValue  see above
    \param maximumValue  see above
    \param stackEnd         0, if not set
    \param dataType         ptr to C-string buffer (min size 16 chars!)

    \returns 0 on success, not 0 in case of error, type of error is
             not yet detailed.
 */
extern bool esriArcView3Only;

extern "C" PCR_DLL_FUNC(int)
    pcrReadEsriDirectoryStackInfo(const char *dirName, int *allMissingValue, double *minimumValue,
                                  double *maximumValue, int *stackEnd, char *dataType)
{
  esriArcView3Only = true;
  try {
    com::PathName pn(dirName);
    pn += "pcr_esri";
    pcrxml::Document const dc(pn);
    pcrxml::DirectoryStackInfo const d(dc.documentElement());

    *allMissingValue = 0;  // FALSE;
    if (d.allMissingValue.present()) {
      *allMissingValue = d.allMissingValue();
    }
    if (!*allMissingValue) {
      *minimumValue = d.minimumValue();
      *maximumValue = d.maximumValue();
    } else {
      *minimumValue = *maximumValue = 0;  // for sanity
    }
    if (d.stackEnd.present()) {
      *stackEnd = d.stackEnd();
    } else {
      *stackEnd = 0;
    }
    PRECOND(d.dataTypeDTD.attrValueStr().size() <= 15);
    strcpy(dataType, d.dataTypeDTD.attrValueStr().c_str());
  } catch (...) {
    return 1;
  }
  return 0;
}
