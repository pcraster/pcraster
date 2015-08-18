#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_TEMPDIRECTORY
#include "com_tempdirectory.h"
#define INCLUDED_COM_TEMPDIRECTORY
#endif
// Module headers.
#ifndef INCLUDED_CALC_ICACHEDOBJECT
#include "calc_icachedobject.h"
#define INCLUDED_CALC_ICACHEDOBJECT
#endif
#ifndef INCLUDED_CALC_IOSTRATEGY
#include "calc_iostrategy.h"
#define INCLUDED_CALC_IOSTRATEGY
#endif
#ifndef INCLUDED_CALC_RUNTIMEENVSETTINGS
#include "calc_runtimeenvsettings.h"
#define INCLUDED_CALC_RUNTIMEENVSETTINGS
#endif
#ifndef INCLUDED_CALC_FIELDWRITER
#include "calc_fieldwriter.h"
#define INCLUDED_CALC_FIELDWRITER
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h" // cast to IFieldRDConversion
#define INCLUDED_CALC_SPATIALPACKING
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_STACKEDVALUE
#include "calc_stackedvalue.h"
#define INCLUDED_CALC_STACKEDVALUE
#endif
#ifndef INCLUDED_CALC_DVAUTOPTR
#include "calc_dvautoptr.h"
#define INCLUDED_CALC_DVAUTOPTR
#endif
#ifndef INCLUDED_CALC_XMLCONTEXT
#include "calc_xmlcontext.h"
#define INCLUDED_CALC_XMLCONTEXT
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h" // setMemoryExchangeData, cast from d_data.symbols
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

/*!
  \file
  This file contains the implementation of the RunTimeEnv class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class RunTimeEnvPrivate
{
public:

  RunTimeEnvPrivate()
  {
  }

  ~RunTimeEnvPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNTIMEENV MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RUNTIMEENV MEMBERS
//------------------------------------------------------------------------------

/*! \brief initialize the runtime environment
 */
void calc::RunTimeEnv::init(const RunTimeEnvSettings& s)
{
  d_stack.clean();
  d_data.clean();

  d_ioStrategy       = new IOStrategy(s.ioStrategy());

  d_enableCache      =d_ioStrategy->mvCompression();

  DataTable::d_useDiskStorage   =s.useDiskStorage();
  d_swapDir=0;
  if (DataTable::d_useDiskStorage) {
    // does not work, will invalidate cache
    // seee mark2 model
    d_enableCache      =false;
    d_swapDir = new com::TempDirectory("pcrcalcSwap");
  }

  d_syncEachTimeStep =d_ioStrategy->writeEachTimeStep();
  d_profile          = s.profile();

  d_timer            =d_ioStrategy->timer();

  d_cellIterator=0;
}

//! elaborate ctor
calc::RunTimeEnv::RunTimeEnv(
    const RunTimeEnvSettings&  s)
{
  init(s);
  checkConstraints(s);
}


/*! simplified ctor with decent defaults
 * \param   rs  fixed geo::RasterSpace,other are the defaults of RunTimeEnvSettings
 * \throws  if !rs.valid()
 */
calc::RunTimeEnv::RunTimeEnv(
    const geo::RasterSpace&  rs)
{
  RunTimeEnvSettings s;
  init(s);
  d_ioStrategy->setRasterSpace(rs);
  checkConstraints(s);
}


calc::RunTimeEnv::~RunTimeEnv()
{
  clean();
}

//! tests constraints for nrRow and nrCell limits
void calc::RunTimeEnv::checkConstraints(
   const RunTimeEnvSettings& s) const
{
  // The CSF rasterformat uses INT4 to store the number of rows or the number of columns
  // the maximum value should not exeed signed int32
  // bug/sf648
  size_t MAX_ROW_COL = 2147483647; // (2 ^ 31) - 1;

  if(d_ioStrategy->rasterSpace().nrRows() > MAX_ROW_COL){
    throw com::Exception("pcrcalc does not support maps holding more than 2^31 - 1 rows");
  }

  if(d_ioStrategy->rasterSpace().nrCols() > MAX_ROW_COL){
    throw com::Exception("pcrcalc does not support maps holding more than 2^31 - 1 colums");
  }
}

//! delete/free all value and set them to 0 (null)
void calc::RunTimeEnv::deleteAllValues()
{
  d_data.clean();
  for(Cache::const_iterator i=d_cache.begin(); i != d_cache.end(); ++i)
   delete i->second;
  d_cache.clear();
}

void calc::RunTimeEnv::clean()
{
  for(Writers::const_iterator i=d_writers.begin(); i != d_writers.end(); ++i)
   delete i->second;
  d_writers.clear();

  deleteAllValues();

  delete d_ioStrategy;
  d_ioStrategy = 0;
  delete d_swapDir;
  d_swapDir = 0;
}

void calc::RunTimeEnv::cleanOnException()
{
  d_stack.clean();
  clean();
}

//! does this holds any data values ?
/*!
 * ManagedScript's with a correct setLastUse(CFGNode *cfg,bool keepLiveAtEnd)
 * should leave an empty() RunTimeEnv() after executing cfg.
 * empty() checks this.
 */
bool calc::RunTimeEnv::empty() const
{
  // all data should be deleted at end of enclosing BasicBlock's
  // cache should be empty in normal cases
  return d_data.allNoValue() && d_cache.empty();
}


/* NOT IMPLEMENTED
//! Assignment operator.
calc::RunTimeEnv& calc::RunTimeEnv::operator=(const RunTimeEnv& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::RunTimeEnv::RunTimeEnv(const RunTimeEnv& rhs):
  Base(rhs)
{
}
*/

/*! create a field with space allocated for the values for immediate use
 *  to store the result of an operation execution (IOpImpl::exec)
 */
calc::Field* calc::RunTimeEnv::createResultField(const DataType& d) const
{
  PRECOND(!d.stEither());
  Field *f =d.stSpatial()
            ? d_ioStrategy->createSpatial(d.vs())
            : new NonSpatial(d.vs());
  f->setReadOnlyReference(false);
  return f;
}

//! get value of rasterSpace
const geo::RasterSpace& calc::RunTimeEnv::rasterSpace() const
{
  return d_ioStrategy->rasterSpace();
}

//! get value of spatialPacking
const calc::SpatialPacking& calc::RunTimeEnv::spatialPacking() const
{
  return d_ioStrategy->spatialPacking();
}

//! get IFieldRDConversion interface
const calc::IFieldRDConversion& calc::RunTimeEnv::ifieldRDConversion() const
{
  return d_ioStrategy->spatialPacking();
}

//! prepare for execution
/*!
 *  this is the start of "destructive" operations such as creating
 *  removing old versions of new outputs, and create writers.
 *  \note when called in need of output time series do call finish() at end
 *        of execution.
 */

void calc::RunTimeEnv::start()
{

  // remove old versions of the file to be written
  for(Writers::const_iterator i=d_writers.begin(); i != d_writers.end(); ++i)
    i->second->remove();
}

//! finish all writers, remove possible swapDir
void calc::RunTimeEnv::finish() {
  Writers::const_iterator i;
  for(i=d_writers.begin(); i != d_writers.end(); ++i)
    i->second->finish();
  delete d_swapDir;
  d_swapDir=0;
}

//! check if value of \a name is already loaded if not, read it
/*!
 * StackedValue::load() needs this
 */
calc::DataValue* calc::RunTimeEnv::load(
    std::string const&   name,
    std::string const&   externalName,
    bool                 lastUse)
{
  DataTable::DTE e(d_data.dataLoad(name));
  DataValue *dv =e.getOrReleaseValue(lastUse);
  if (dv) // yes loaded
    return dv;

  // not loaded, FTTB only possible in case of fields
  PRECOND(isIn(e.symbol().vs(),VS_FIELD));
  // not yet loaded, read it external
  Field *s(0);
  try {
   s=d_ioStrategy->createReadField(externalName, e.symbol().dataType());
  } catch(const com::Exception& ex) {
      e.symbol().throwAtFirst(ex);
  }
  POSTCOND(s);

  // DISK/MEMORY SWITCH
  if (DataTable::d_useDiskStorage)
    return s;
  if (!lastUse) {
    // store in d_data for later use,
    s->setReadOnlyReference(true);
    PRECOND(!e.dataValue());
    e.dataValue()=s;
  }
  return s;
}

size_t calc::RunTimeEnv::stackSize() const
{
  return d_stack.size();
}

//! get value of \a p from d_data and push it on one of the stacks.
void calc::RunTimeEnv::pushValue(const ASTPar* p)
{
  DataTable::DTE e(d_data.dataLoad(p->name()));
  d_stack.push(new StackedValue(*this,e.symbol(),p->lastUse()));
}

//! push a DataValue, ownership depends
/*!
 * Not really const; ownership (e.g. transfer \a d to
 * RunTimeEnv) and readOnlyReference() is set by DataValue itself.
 */
void calc::RunTimeEnv::pushDataValue(
    const DataValue *d)
{
  d_stack.push(const_cast<DataValue *>(d));
}

//! cast wrapper on pushDataValue(const DataValue *f)
void calc::RunTimeEnv::pushField(const Field *f)
{
  pushDataValue(f);
}

//! pop a DataValue, possibly releasing owner ship
/*!
 * \returns a DataValue
 * Ownership is only released under the conditions
 * as present in deleteFromPcrme(const DataValue). In other
 * words: use deleteFromPcrme to clean up the pointer returned.
 *
 */
calc::DataValue* calc::RunTimeEnv::popDataValue()
{
  return d_stack.pop();
}

//! pop a field, possibly releasing owner ship
/*!
 *  simple cast wrapper around RunTimeEnv::popDataValue().
 */
calc::Field* calc::RunTimeEnv::popField()
{
  Field *f = dynamic_cast<Field *>(popDataValue());
  POSTCOND(f);
  return f;
}

//! return true if stack has no false (only true or MV's)
bool calc::RunTimeEnv::stackedCondition()
{
  DVAutoPtr<Field> f(popField());
  bool noneAreTrue,noneAreFalse;
  f->analyzeBoolean(noneAreTrue,noneAreFalse);
  return noneAreFalse;
}

//! update output tss with name \a tss, delete id and expr
void calc::RunTimeEnv::assignOutTss(
    const std::string& tss)
{
  // reverse stack order
  DVAutoPtr<Field> expr(popField());
  DVAutoPtr<Field> id  (popField());

  d_writers[tss]->writeOutTss(id.get(),expr.get(),d_timer.currentInt());
}

void calc::RunTimeEnv::deleteCacheEntry(const void* fieldSrcValue)
{
  Cache::iterator pos=d_cache.find(fieldSrcValue);
  if (pos != d_cache.end()) {
    delete pos->second;
    d_cache.erase(pos);
  }
}

void calc::RunTimeEnv::deleteValue(
    const std::string& parName)
{
  PRECOND(d_data.contains(parName));
  DataTable::DTE e(d_data.dataLoad(parName));
  Field *f = dynamic_cast<Field *>(e.dataValue());
  if (f) {
    // a loaded field can have cached objects
    deleteCacheEntry(f->src());
  }
  deleteAlways(e.dataValue());
  e.dataValue()=0;
}


//! find an ICachedObject by Field::src value
const calc::ICachedObject* calc::RunTimeEnv::cachedObject(
    const void*    fieldSrcValue)
{
    Cache::const_iterator pos=d_cache.find(fieldSrcValue);
    if (pos != d_cache.end())
      return pos->second;
    return 0;
}

//! update and transfer \a obj in cache if cached is enabled
void calc::RunTimeEnv::transferIfCached(
    const void*    fieldSrcValue,
    const ICachedObject *obj)
{
  // multiple delete problem
  Cache::iterator pos=d_cache.find(fieldSrcValue);
  if (pos != d_cache.end()) {
    // a previous update may already toke place
    if (pos->second == obj)
      return;
  }
  // multiple delete problem
  deleteCacheEntry(fieldSrcValue);
  if (d_enableCache)
   d_cache[fieldSrcValue] = obj;
}

void calc::RunTimeEnv::transferMemoryExchangeItemIntoDataTransferArray(
  MemoryExchangeItem* item)
{
  d_ioStrategy->transferMemoryExchangeItemIntoDataTransferArray(item);
}

#ifndef INCLUDED_CALC_DISKWRITTENFIELD
#include "calc_diskwrittenfield.h"
#define INCLUDED_CALC_DISKWRITTENFIELD
#endif
#ifndef INCLUDED_CALC_GRIDSTAT
#include "calc_gridstat.h"
#define INCLUDED_CALC_GRIDSTAT
#endif

//! assign the stacked result to this \a par
/*!
 * \todo
 *   pcrcalc214c should be resolvable in BuildTypesVisitor
 */
void calc::RunTimeEnv::assignStackTop(
    const ASTPar *p)
{

  DataTable::DTE e(d_data.dataLoad(p->name()));

  // tss is done in assignOutTss
  // object assignment is rewritten in parser
  PRECOND(isIn(e.symbol().vs(),VS_FIELD));


  DVAutoPtr<Field> f(popField());

  // pcrcalc214c FIX, Still needed??
  //    wrsl. wel for python link, maar assignStackTop is geen API voor Kor
  f->resetVs(e.symbol().vs());

  debugMVAssignments(f.get());

  std::string name(e.symbol().name());
  std::string writtenAsFile;

// #error check if must be written
// #test if tss can be written sparse (-1 and normal)
  // if reported at this point in code
  if (p == e.symbol().reportPar()) {
    PRECOND(d_writers.count(name));
    writtenAsFile= d_writers[name]->write(f.get(),d_timer.currentInt());
  }

  if (p->lastUse()) {
   // delete it!
   deleteValue(p->name());
  } else {
   // set to the new value
    if (d_swapDir && f->isSpatial()) {
      if (writtenAsFile.empty()) {
        writtenAsFile=d_swapDir->memberPath(name).string();
        ioStrategy().writeField(writtenAsFile,f.get());
      }
      e.resetValue(
       new DiskWrittenField(ioStrategy(),writtenAsFile,e.symbol().vs()));
   } else
    e.resetValue(f.release());
  }
}

//! assign the stacked results to these \a pars
void calc::RunTimeEnv::assignStackTop(
    const std::vector<ASTPar *> pars)
{
  for(size_t p=0; p < pars.size(); ++p)
    assignStackTop(pars[p]);
}

//! load symbol \a i in DataTable and setup a FieldWriter if needed
void calc::RunTimeEnv::load(const ASTSymbolInfo& i)
{
  bool write =i.reportPosition()!= RPNone;
  bool outTss(false);
  if (i.vs()==VS_TSS) {
    if (i.memoryOutputId() == i.noMemoryExchangeId() && write)
     outTss = true;
  }

  if (!outTss)
    d_data.insert(i, d_timer.lastInt(), *d_ioStrategy);

  if (write) {
    i.checkOneOutputVs();
    try {
        d_writers.insert(std::make_pair(i.name(),
           d_ioStrategy->createFieldWriter(i)));
    } catch(const com::Exception& e) {
      i.throwSym(*(i.reportPar()->position()),e.messages());
    }
  }
}

void calc::RunTimeEnv::setMemoryExchangeData(void **data)
{
 d_data.setMemoryExchangeInputData(data);

 // keep  data here:
 d_ioStrategy->setMemoryExchangeData(d_data.symbols(),data);
}

const calc::DataTable& calc::RunTimeEnv::dataTable() const
{
  return d_data;
}

//! get value of d_ioStrategy
const calc::IOStrategy& calc::RunTimeEnv::ioStrategy() const
{
  return *d_ioStrategy;
}

//! set value of d_syncEachTimeStep
void calc::RunTimeEnv::setSyncEachTimeStep(bool syncEachTimeStep)
{
  d_syncEachTimeStep=syncEachTimeStep;
}

//! get value of d_syncEachTimeStep
bool calc::RunTimeEnv::syncEachTimeStep() const
{
  return d_syncEachTimeStep;
}

void calc::RunTimeEnv::debugMVAssignments(
    const Field* f) const
{
  if (f->isSpatial())
    d_ioStrategy->debugMVAssignments(f);
}

//! set value of d_cellIterator
void calc::RunTimeEnv::setCellIterator(ICellIterator* cellIterator)
{
  d_cellIterator=cellIterator;
}

//! get value of d_cellIterator
calc::ICellIterator* calc::RunTimeEnv::cellIterator() const
{
  return d_cellIterator;
}

//! set value of d_timer
void calc::RunTimeEnv::setTimer(const Timer& timer)
{
  d_timer=timer;
}

//! get value of d_timer
const calc::Timer& calc::RunTimeEnv::timer() const
{
  return d_timer;
}


void calc::RunTimeEnv::incCurrentTimeStep()
{
  d_timer.increment();
}
//! get value of ioStrategy, to modify
calc::IOStrategy& calc::RunTimeEnv::ioStrategy()
{
  return *d_ioStrategy;
}

pcrxml::RunContext* calc::RunTimeEnv::createXMLContext() const
{
  return createXMLRunContext(ioStrategy().areaMap(),timer());
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



