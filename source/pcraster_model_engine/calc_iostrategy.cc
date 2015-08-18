#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif
// Module headers.
#ifndef INCLUDED_CALC_XMLREFLECTION
#include "calc_xmlreflection.h"
#define INCLUDED_CALC_XMLREFLECTION
#endif
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif
#ifndef INCLUDED_CALC_AREAMAP
#include "calc_areamap.h"
#define INCLUDED_CALC_AREAMAP
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_CALC_ASISPACKING
#include "calc_asispacking.h"
#define INCLUDED_CALC_ASISPACKING
#endif
#ifndef INCLUDED_CALC_MASKPACKING
#include "calc_maskpacking.h"
#define INCLUDED_CALC_MASKPACKING
#endif
#ifndef INCLUDED_CALC_UNPACKEDCREATION
#include "calc_unpackedcreation.h"
#define INCLUDED_CALC_UNPACKEDCREATION
#endif
#ifndef INCLUDED_CALC_UNPACKEDSRC
#include "calc_unpackedsrc.h"
#define INCLUDED_CALC_UNPACKEDSRC
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_GRIDSTAT
#include "calc_gridstat.h"
#define INCLUDED_CALC_GRIDSTAT
#endif
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif
#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif
#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEM
#include "calc_MemoryExchangeItem.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEM
#endif
#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD
#include "calc_MemoryExchangeItemField.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD
#endif


/*!
  \file
  This file contains the implementation of the IOStrategy class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class IOStrategyPrivate
{
public:

  IOStrategyPrivate()
  {
  }

  ~IOStrategyPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOSTRATEGY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF IOSTRATEGY MEMBERS
//------------------------------------------------------------------------------

namespace calc {

IOStrategy::IOStrategy():
  d_fs(new IoFieldStrategy(appIOstrategy)),
  d_areaMap(new AreaMap()),
  d_spatialPacking(0),
  d_mvCompression(false),
  d_writeEachTimeStep(false)
{
}



//! Copy constructor.
IOStrategy::IOStrategy(
         IOStrategy const& rhs):
  d_fs                   (new IoFieldStrategy(*(rhs.d_fs))),
  d_areaMap              ( new AreaMap(*rhs.d_areaMap)),
  d_spatialPacking       ( com::non0Clone(rhs.d_spatialPacking)),
  d_runDirectory         ( rhs.d_runDirectory),
  d_timer                ( rhs.d_timer),
  d_memoryData           (rhs.d_memoryData),
  d_debugMVAssignmentsMap(rhs.d_debugMVAssignmentsMap),
  d_mvCompression        (rhs.d_mvCompression),
  d_writeEachTimeStep    (rhs.d_writeEachTimeStep)
{
}

IOStrategy::~IOStrategy()
{
  delete d_fs;
  delete d_areaMap;
  delete d_spatialPacking;
}


/* NOT IMPLEMENTED
//! Assignment operator.
IOStrategy& IOStrategy::operator=(
         IOStrategy const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void  IOStrategy::setXMLAreaMapScript(
    pcrxml::AreaMapScript const& areaMapScript)
{
    delete d_areaMap;
    d_areaMap = new AreaMap(areaMapScript);
}

void  IOStrategy::setXMLComputationMask(
    pcrxml::ComputationMask const& c)
{
    d_areaMap->setComputationMask(c);
}

/*
 * void IOStrategy::updateXML(XMLReflection& xr) const
 * {
 *   delete xr.exchangeModel().areaMapDTD;
 *   xr.exchangeModel().areaMapDTD = 0;
 *   if (d_areaMap->isSet())
 *     xr.exchangeModel().areaMapDTD = d_areaMap->createXML();
 *   xr.exchangeModel().ioStrategy = ioStrategyType();
 * }
 * void  IOStrategy::configureSymbols(
 *     ASTSymbolTable& symbols,
 *     const XMLReflection& xr)
 * {
 *   if (xr.exchangeModel().areaMapDTD) {
 *     delete d_areaMap;
 *     d_areaMap = new AreaMap(*(xr.exchangeModel().areaMapDTD));
 *   }
 *   for(ASTSymbolTable::iterator i=symbols.begin(); i!=symbols.end();++i) {
 *     pcrxml::ExchangeItem *c=xr[i->first];
 *     if (c)
 *      i->second.configure(*c);
 *   }
 * }
 * pcrxml::IOStrategyType IOStrategy::ioStrategyType()const
 * {
 *   return pcrxml::IOStrategyType::IOFiles;
 * }
 */

/* set TODO
 * \param symbols symbols that may have an memory id
 * \param dataTransferArray reference to raw user's DataTransferArray
 *                          passed in by LinkIn call. During execution
 *                          this array is changed if the user wants allocation
 */
void IOStrategy::setMemoryExchangeData(const ASTSymbolTable& symbols,
                                       void  **dataTransferArray)
{
  PRECOND(dataTransferArray);

  d_memoryData.clear();
  d_dataTransferArray = dataTransferArray;
  d_dataTransferArrayUser0.clear();

  // fill d_memoryData with an MemoryExchangeItem value
  //  if output value are going to allocated per user request
  //  this value is replaced by one of the sub classes.

  const size_t noExchange(ASTSymbolInfo::noMemoryExchangeId());
  int maxIdInt=-1;
  BOOST_FOREACH(ASTSymbolTablePair i, symbols) {
    ASTSymbolInfo const& si(i.second);
    size_t ids[2];
    ids[0]=si.memoryInputId();
    ids[1]=si.memoryOutputId();
    if(ids[0] != noExchange || ids[1] != noExchange) {
      for(int id=0; id<2; ++id) {
        // not both set
        PRECOND(ids[0] != noExchange || ids[1] != noExchange);
        if(ids[id] != noExchange) {
          maxIdInt = std::max<int>(maxIdInt,(size_t)ids[id]);
          d_memoryData.insert(std::make_pair(si.name(),
            boost::shared_ptr<MemoryExchangeItem>(
              new MemoryExchangeItem(si.name(),ids[id]))));
        }
      }
    }
  }

  if (maxIdInt != -1)
  {
    d_dataTransferArrayUser0 = std::vector<bool>(maxIdInt+1);
    for(size_t i=0; i < d_dataTransferArrayUser0.size(); ++i)
      d_dataTransferArrayUser0[i] = d_dataTransferArray[i] == 0;
  }
}

/*!
 * \param symbols updated with type info possibly found.
 * \param areaMap name of areaMap if set, empty() otherwise
 * \param timer d_timer is reset with \a timer
 *
 * Update \a d_areaMap (clone) if needed()
 *
 * \todo
 *   if d_compression but areamap has all true values
 *   do not install spatialpacking
 */
void  IOStrategy::resolve(
    ASTSymbolTable&      symbols,
    std::string const& areaMap,
    Timer const& timer)
{
  d_timer=timer;

  // check if debug map is specified that it is not used as a symbol also
  if ( (!d_debugMVAssignmentsMap.empty()) && symbols.contains(d_debugMVAssignmentsMap))
    throw com::FileError(d_debugMVAssignmentsMap,
        "result map for -d can not be used in the script");

  bool needExplicitClone=d_mvCompression || !d_debugMVAssignmentsMap.empty();
  if (needExplicitClone) {
    // need explicit clone
    // if not there trick a AreaMap::throwIfNotSet by passing empty RS
    if (areaMap.empty())
        d_areaMap->setRasterSpace(geo::RasterSpace());
  }

  initResolve();

  // resolve all
  //  - what's now calc::ASTSymbolTable::resolve()
  //  - Field's are redirected to Memory or Files
  //  - Input tss and tables are always done by files. DataTable::insert not earlier
  // WARNING: this resolve calls back into this, thus symbols is NOT stable
  //          DO NOT replace this loop with a FOR_EACH construct.
  for(ASTSymbolTable::iterator i=symbols.begin(); i!=symbols.end();++i)
    i->second.resolve(*this);

  // see if we have picked up a RasterSpace from resolved symbols (Files)
  //  --> e.g. implicit clone
  d_areaMap->throwIfNotSet(); // pcrcalc4

  if (!d_spatialPacking)
    d_spatialPacking=new AsIsPacking(rasterSpace());
  if (needExplicitClone) {
     PRECOND(!areaMap.empty());
     Field* f =createReadField(
       symbols[areaMap].externalName(),
       symbols[areaMap].dataType());
    d_areaMap->transferMask(f);
  }
  if (d_areaMap->hasCoordinateMask()) {
    d_areaMap->setMaskOnCoordinates();
  }
  if (d_mvCompression || d_areaMap->hasCoordinateMask() ) {
    delete d_spatialPacking;
    d_spatialPacking=new MaskPacking(rasterSpace(),d_areaMap->mask());
  }
}

void IOStrategy::debugMVAssignments(const Field* f) const
{
  if (d_debugMVAssignmentsMap.empty())
    return;

  Field *mark = f->findMVinMask(d_areaMap->mask());
  if (mark) {
    writeField(d_debugMVAssignmentsMap,mark);
    deleteFromPcrme(mark);
    throw com::Exception(
         "-d catched MV creation, inspection map written to "
         +d_debugMVAssignmentsMap);
  }
}

void IOStrategy::setRasterSpace(const geo::RasterSpace& rs)
{
  d_areaMap->setRasterSpace(rs);
  delete d_spatialPacking;
  d_spatialPacking=new AsIsPacking(rs);
}

void  IOStrategy::initResolve()
{
  // parsing of file may recreate this
  // e.g. #! --esrigrid
  delete d_fs;
  d_fs=new IoFieldStrategy(appIOstrategy);

  d_fs->setRasterSpace(rasterSpace());
}

void IOStrategy::checkOutputFilePath(ASTSymbolInfo const& i)
{
  if (isIn(i.vs(),VS_FIELD) && memoryValue(i.name()))
  { // Output field will be MemoryExchange'd no output file path
    return;
  }
  else
    d_runDirectory.checkOutputFilePath(i.externalName());
}

std::string IOStrategy::outputFilePath(const std::string& fileName) const
{
  return d_runDirectory.outputFilePath(fileName);
}

//! \todo ONLY called for files not memory, correct place?
calc::DataType IOStrategy::resolveInputSymbol(
    std::string&           newExternalName,
    const        DataType& dt)
{
  bool foundDummy;
  newExternalName=d_runDirectory.inPath(foundDummy,newExternalName);

  // calc::File file(fileName);
  // // will throw if not exists:
  // file.validateExisting();

  bool expectField=isIn(dt.vs(),VS_FIELD);
  DataType newDt(dt.vs());
  try {
     // updates vs
     newDt=resolveInputField(newExternalName,dt);
  } catch(const com::FileFormatError& ) {
     if (expectField)
      throw com::Exception("Expected map, got a different file");
    else
      return newDt; // know nothing more
  }
  // it is a map
  if (expectField) // as expected
     return newDt;
  // but expecting something else
  std::ostringstream os;
  os << "Expected " << dt.vs() <<", got a map";
  throw com::Exception(os.str());
  return newDt;
}

Field* IOStrategy::createReadSpatial(const std::string& mapName,
                                     VS vs) const
{
  return createReadField(mapName,DataType(vs,ST_SPATIAL));
}

//! create a field and read contents of mapName into it
Field* IOStrategy::createReadField(const std::string& mapName,
                             const DataType& type)const
{
  switch(type.st()) {
    case ST_SPATIAL: {
        UnpackedCreation u(*d_spatialPacking,type.vs());
        readField(u.unpackedDest(),mapName,type);
        return u.releasePacked();
    }
    case ST_NONSPATIAL: {
        NonSpatial *ns = new NonSpatial(type.vs());
        try {
         readField(ns->dest(),mapName,type);
        } catch(...) {
          delete ns;
          throw;
        }
        return ns;
    }
    default:;
  }
  POSTCOND(false);
  return 0;
}

/*! return 0 if not in d_memoryData
 *  throw com::Exception if in d_memoryData but 0 ptr.
 */
/*
void* IOStrategy::memoryValue(std::string const& name) const {
  MemoryData::const_iterator i=d_memoryData.find(name);
  if(i == d_memoryData.end())
    return 0;
  size_t id = i->second->id();
  if (!d_dataTransferArray[id]) {
  return d_dataTransferArray[id];
}
*/

/*! return 0 if not in d_memoryData
 *  throw com::Exception if in d_memoryData but 0 ptr.
 */
MemoryExchangeItem* IOStrategy::memoryValue(std::string const& name) const {
  MemoryData::const_iterator i = d_memoryData.find(name);
  if (i != d_memoryData.end())
  {
    PRECOND(i->second.get()); // always has a value
    return i->second.get();
  }
  return 0;
}

void IOStrategy::transferMemoryExchangeItemIntoDataTransferArray(
        MemoryExchangeItem* i)
{
    size_t id(i->id());
    if (!(id < d_dataTransferArrayUser0.size())) {
     PRINT_VAR(id);
     PRINT_VAR(d_dataTransferArrayUser0.size());
     PRECOND(id < d_dataTransferArrayUser0.size());
    }

    if (!d_dataTransferArrayUser0[id])
    {  // user passed a valid buffer
       PRECOND(d_dataTransferArray[id]);
       i->beMemCpySrc(d_dataTransferArray[id]);
    }
    else {
      // user is requesting us to allocate it
      // do it and keep hanging it around in d_memoryData

      // will clean up previous MemoryExchangeItem
      d_memoryData[i->name()] =
       boost::shared_ptr<MemoryExchangeItem>(i);

      // change user's d_dataTransferArray
      d_dataTransferArray[id] = (void *)(i->rawValue());
   }
}

void  IOStrategy::readField(
   void *dest,
   const std::string& name,
   const DataType& type) const
{
  MemoryExchangeItem *mem = memoryValue(name);
  if (!mem) {
    // file based
    PRECOND(type.st()==ST_SPATIAL);
    d_fs->readField(dest,name,type.vs());
    return;
  }

  // memory exchange
  const void *src = d_dataTransferArray[mem->id()];
  if (src)
  {
   size_t n(type.st()==ST_SPATIAL ? rasterSpace().nrCells() : 1);
   std::memcpy(dest, src, n*bytesPerCell(type.vs()));
  } else {
   throw com::Exception("0-ptr data input buffer passed");
  }
}

GridStat IOStrategy::writeFieldUnpacked(
    const std::string& name,
    const Field *f)
{
  MemoryExchangeItem* mem = memoryValue(name);

  if (!mem)
    return d_fs->writeFieldUnpacked(name,f);
  else
  { // write to memory
    if (!d_dataTransferArrayUser0[mem->id()])
    { // user supplied buffer
      PRECOND(d_dataTransferArray[mem->id()]);
      f->beMemCpySrc(d_dataTransferArray[mem->id()]);
    } else {
      // user is requesting us to allocate it
      PRECOND(mem->name() == name);
      boost::shared_ptr<Field> allocatedCopy(f->createClone());
      MemoryExchangeItemField *mei =
       new MemoryExchangeItemField(name, mem->id(),
                                allocatedCopy);
      d_memoryData[name] = boost::shared_ptr<MemoryExchangeItem>(mei);
      // change user's d_dataTransferArray
      d_dataTransferArray[mei->id()] = (void *)(allocatedCopy->src());
    }
    return GridStat();
  }
}

DataType IOStrategy::resolveInputField(
    const std::string& newExternalName,
    const DataType&    dt)
{
  VS vs=dt.vs();
  d_fs->checkInputMap(vs,newExternalName);
  // pick the found rasterSpace
  setRasterSpace(d_fs->rasterSpace());
  return DataType(vs,ST_SPATIAL);
}

/*
 * \todo Due to updating d_memoryData in writeFieldUnpacked the const'ness
 *       of this method is hacked away.
 */
GridStat calc::IOStrategy::writeField(
    const std::string& fileName,
    const Field *f) const
{
  IOStrategy *hack = (IOStrategy *)this;
  if (f->isSpatial()) {
    UnpackedSrc us(*d_spatialPacking, f);
    PRECOND(us.src()->nrValues()==rasterSpace().nrCells());
    return hack->writeFieldUnpacked(fileName,us.src());
  } else
    return hack->writeFieldUnpacked(fileName,f);
}

Field* IOStrategy::createSpatial(VS vs) const
{
  return d_spatialPacking->createSpatial(vs);
}

//! set value of d_runDirectory
void IOStrategy::setRunDirectory(const com::PathName& runDirectoryPath)
{
  d_runDirectory= RunDirectory(runDirectoryPath);
}

//! get value of d_runDirectory
const RunDirectory& IOStrategy::runDirectory() const
{
  return d_runDirectory;
}


//! get value of d_timer
const Timer& IOStrategy::timer() const
{
  return d_timer;
}


//! set value of d_writeEachTimeStep
void calc::IOStrategy::setWriteEachTimeStep(bool writeEachTimeStep)
{
  d_writeEachTimeStep=writeEachTimeStep;
}

//! get value of d_writeEachTimeStep
bool calc::IOStrategy::writeEachTimeStep() const
{
  return d_writeEachTimeStep;
}

//! set value of d_debugMVAssignmentsMap
void calc::IOStrategy::setDebugMVAssignmentsMap(const std::string& debugMVAssignmentsMap)
{
  d_debugMVAssignmentsMap=debugMVAssignmentsMap;
}

//! set value of d_mvCompression
void calc::IOStrategy::setMVCompression(bool mvCompression)
{
  d_mvCompression=mvCompression;
}

//! get value of d_debugMVAssignmentsMap
const std::string& calc::IOStrategy::debugMVAssignmentsMap() const
{
  return d_debugMVAssignmentsMap;
}

//! get value of d_mvCompression
bool calc::IOStrategy::mvCompression() const
{
  return d_mvCompression;
}

//! get value of d_areaMap
const AreaMap& IOStrategy::areaMap() const
{
  return *d_areaMap;
}

const geo::RasterSpace& IOStrategy::rasterSpace() const
{
  return d_areaMap->rasterSpace();
}

/*
 * tricky get rid of, or return a default 1:1 for iFieldRDConversion
 */
const SpatialPacking& IOStrategy::spatialPacking() const
{
  PRECOND(d_spatialPacking);
  return *d_spatialPacking;
}


//! return 0 if can not be created yet due to lack of timer
StackInput* IOStrategy::createStackInput(
    const std::string& externalName,
    const MapStackType& type)
{
  std::string inPath=d_fs->inPathStack(runDirectory(),externalName,timer().lastInt());

  if (type.use() == MapStackType::Lookup) // no need for timer
    return new StackInput(*this,inPath,type);

  if (timer().lastInt()==0)
    return 0;
  return new StackInput(*this,inPath,type);
}

void IOStrategy::setStackInfo(const StackInfo& s) const
{
  d_fs->setStackInfo(s);
}

std::string IOStrategy::makeStackItemName(const std::string& iname,
                                       int   atTimeStep) const
{
  return d_fs->makeStackItemName(iname,atTimeStep);
}


IoFieldStrategy& IOStrategy::ioFieldStrategy() {
  return *d_fs;
}




} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
