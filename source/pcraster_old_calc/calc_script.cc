#include "stddefx.h"

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_CALC_SCRIPT
#include "calc_script.h"
#define INCLUDED_CALC_SCRIPT
#endif

#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif

#ifndef INCLUDED_CALC_USEPAR
#include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif

#ifndef INCLUDED_CALC_SYMBOLTABLE
#include "calc_symboltable.h"
#define INCLUDED_CALC_SYMBOLTABLE
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"     // FieldHandle
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#include "calc_fieldmapinputparameter.h"
#define INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#endif
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

#ifndef INCLUDED_CALC_USERSYMBOL
#include "calc_usersymbol.h"
#define INCLUDED_CALC_USERSYMBOL
#endif

#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif

#ifndef INCLUDED_CALC_PARSPAR
#include "calc_parspar.h"
#define INCLUDED_CALC_PARSPAR
#endif

#ifndef INCLUDED_CALC_EXTERNALSYMBOLS
#include "calc_externalsymbols.h"
#define INCLUDED_CALC_EXTERNALSYMBOLS
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

#ifndef INCLUDED_EXTERNFUNCS
#include "externfuncs.h"
#define INCLUDED_EXTERNFUNCS
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#include "calc_quitforprogresscallback.h"
#define INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#endif
#ifndef INCLUDED_CALC_QUITFOREXITOPTION
#include "calc_quitforexitoption.h"
#define INCLUDED_CALC_QUITFOREXITOPTION
#endif
#ifndef INCLUDED_PCRGENXML_SCRIPT
#include "pcrgenxml_script.h"
#define INCLUDED_PCRGENXML_SCRIPT
#endif

#ifndef INCLUDED_CALC_TIMETABLE
#include "calc_timetable.h"
#define INCLUDED_CALC_TIMETABLE
#endif

#ifndef INCLUDED_CALC_PROGRESSCALLBACK
#include "calc_progresscallback.h"
#define INCLUDED_CALC_PROGRESSCALLBACK
#endif

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif

#ifndef INCLUDED_CALC_POSITIONNONE
#include "calc_positionnone.h"
#define INCLUDED_CALC_POSITIONNONE
#endif

#ifndef INCLUDED_CALC_MASKCOMPRESSOR
#include "calc_maskcompressor.h"
#define INCLUDED_CALC_MASKCOMPRESSOR
#endif

#ifndef INCLUDED_CALC_NULLCOMPRESSOR
#include "calc_nullcompressor.h"
#define INCLUDED_CALC_NULLCOMPRESSOR
#endif

namespace calc {

static ProgressCallBack defaultProgressCallBack;
}


/* \brief ctor
 */
calc::Script::Script():
    StatementBlock(generatedSymbol("script","script"),0),
    d_progressCallBack(&defaultProgressCallBack),
    d_exitValueType(ALWAYS_0),
    d_fileOutputStream(0),
    d_compression(false),
    d_0compression(false),
    d_exitVal(0),
    d_writeEachTimeStep(false),
    d_areaMask(0),
    d_compressor(0),
    d_aReportFound(false),
    d_areaMap(0)
{
 d_ioFieldStrategy = IoFieldStrategy::createOnGlobalOption();
 d_symTab          = new SymbolTable(0);
 // Make sure the singleton instance is clean.
 ExternalSymbols::clear();
}

calc::Script::~Script()
{
    delete [] d_areaMask;
    delete d_areaMap;
    delete d_compressor;
    ExternalSymbols::clear();
    delete d_symTab;
    delete d_ioFieldStrategy;
}

//! see Script::d_fileOutputStream
void calc::Script::setFileOutputStream(std::ostream* fileOutputStream)
{
  d_fileOutputStream = fileOutputStream;
}

//! output html description file (broken;incomplete)
void calc::Script::setHtmlFile(const std::string&  htmlFile)
{
  PRECOND(!htmlFile.empty());
  d_htmlFile=htmlFile;
}

//! output xml description file
void calc::Script::setXmlFile(const std::string&  xmlFile)
{
  PRECOND(!xmlFile.empty());
  d_xmlFile=xmlFile;
}

void calc::Script::setScriptFileName(const com::PathName& scriptFileName)
{
  d_scriptFileName=scriptFileName;
}

//! activate the debugging of assignments
void calc::Script::setDebugMap(const std::string&  debugMapName)
{
  PRECOND(!debugMapName.empty());
  d_debugMapName=debugMapName;
}

void calc::Script::setWriteEachTimeStep(bool enable)
{
  d_writeEachTimeStep=enable;
}

void calc::Script::setMVCompression (bool enable)
{
 d_compression=enable;
}

void calc::Script::set0Compression (bool enable)
{
 d_0compression=enable;
}

//! set report of exit value
void calc::Script::setExitValueType(ExitValueType exitValueType)
{
 d_exitValueType=exitValueType;
}

//! set run directory, will activate the Model Run Organizer
/*!
   \arg runDirectory must be a directory for the Model Run Organizer
   the RunDirectory constructor must verify runDirectory, empty if
   no directory set
   \arg externalBindingsFile external bindings file, empty is not set
*/
void calc::Script::setRunDirectory(const com::PathName& runDirectory,
                                   const com::PathName&  externalBindingsFile)
{
  d_runDirectory.setRunDirectory(runDirectory, externalBindingsFile);
  d_bindingTable.setExternalBindings(this,d_runDirectory);
}

/*! set/resets the strategy
   \todo
      avoid al this rechecking by reworking scanner as such that we
      <ul>
       <li> can
            parse/scan up to the global options, so we can start really
            parsing with options set.</li>
      </ul>
      Create a ScriptConfiguration object that does all the settings
      and peeks into the script. This also allows to initiate the Script
      while no settings can be changed anymore.
 */
void calc::Script::recheckIoFieldStrategy() const
{
  PRECOND(d_ioFieldStrategy);
  if (appIOstrategy != d_ioFieldStrategy->strategyType()) {
    delete d_ioFieldStrategy;
    d_ioFieldStrategy=0;
    d_ioFieldStrategy = IoFieldStrategy::createOnGlobalOption();
 }
}

calc::IoFieldStrategy& calc::Script::ioFieldStrategyMod()
{
  recheckIoFieldStrategy();
  PRECOND(d_ioFieldStrategy);
  return *d_ioFieldStrategy;
}

//! return ioFieldStrategy
const calc::IoFieldStrategy& calc::Script::ioFieldStrategy() const
{
  recheckIoFieldStrategy();
  PRECOND(d_ioFieldStrategy);
  return *d_ioFieldStrategy;
}


bool calc::Script::esriGridIO() const
{
  return ioFieldStrategy().strategyType() == APP_IO_ESRIGRID;
}

bool  calc::Script::writeEachTimeStep() const
{
    return d_writeEachTimeStep;
}

bool  calc::Script::debugMvAssignments() const
{
    return !d_debugMapName.isEmpty();
}

bool  calc::Script::zeroCompression() const
{
    return d_0compression;
}


std::string calc::Script::debugMapName() const
{
    PRECOND(!d_debugMapName.isEmpty());
    return d_debugMapName.toString();
}

unsigned char *calc::Script::areaMask() const
{
    return d_areaMask;
}

const calc::Symbol* calc::Script::findBinding(const std::string& name) const
{
  return d_bindingTable.find(name);
}


//! add a binding
/*!
 * \param vs if given as a typed constant
 */
void calc::Script::addBinding(const Symbol& left, const Symbol& right, VS vs)
{
    d_bindingTable.add(left,right,vs);
}

/*! evaluate current bindings and move to those attched to a
    (constant) number to a parameter
 */
void calc::Script::evaluateBindings()
{
  std::vector<UserSymbol *>
    newPars(d_bindingTable.moveConstantToParameters(this));
  for(size_t i=0; i< newPars.size(); i++)
    addSymbol(newPars[i]);
}


//! add symbol to symbol table of whole script
/*!  if duplicate then delete

    \todo
       can we remove many calls to this method, by letting all
       ctors of params add themselves?
 */
void calc::Script::addSymbol(UserSymbol *newPar)
{
    d_symTab->add(newPar);
}

void calc::Script::run()
{
    d_runDirectory.setupForExecution();

    // start indicator
    updateProgress(LoopPulse,0);

    d_symTab->goInScope();

    calc::StatementBlock::run();

    // completion indicator
    updateProgress(LoopPulse,nrTimeSteps()+1);
}

void calc::Script::executeBlock()
{
    executeStatements();
}

calc::UserSymbol *calc::Script::findSymbol(const calc::Symbol* sym,
        VS typesExpected, bool mustExist) const
{
    return d_symTab->find(sym, typesExpected, mustExist);
}

calc::SubParameter *calc::Script::findRightParameter(
          const ParsPar& par,
        VS expectedVs ) const
{
    return d_symTab->findRightParameter(par, expectedVs);
}

calc::SubParameter *calc::Script::findLeftParameter(
          const ParsPar& par,
        VS expectedVs ) const
{
    return d_symTab->findLeftParameter(par, expectedVs);
}

void calc::Script::setTimer(
    size_t start,
    size_t end,
    size_t slice)
{
  d_timerStart = start;
  d_timerEnd   =   end;
  d_timerSlice = slice;
  POSTCOND(d_timerStart == 1);
  POSTCOND(d_timerSlice == 1);
  POSTCOND(d_timerStart <= d_timerEnd);
}

void calc::Script::setTimer(
    const Symbol& tssIn)
{
 BindedSymbol tss(tssIn);
 tss.setInputFilePath();
 try {
  TimeTable tt(tss.externalName());
  d_timerStart = 1;
  d_timerEnd   = tt.nrTimeSteps();;
  d_timerSlice = 1;
 } catch(const com::Exception& e) {
   tss.posError(e.messages());
 }
}

//! adjust and check model tree
void calc::Script::buildScript()
{
   // when building the script
   // BuildType is already called
   // NOTE that with multiple retyping only the spatial vs. non-spatial
   // should be adjusted, thus type-related syntax error are only found
   // in the first run. (when building the tree)

   // Now call it again, ONLY to fix for
   // non-spatial -> spatial promotion
   while (buildTypes())
       /* just do buildTypes 'till solved */;    // pcrcalc/test60

   prepareExecution();

   d_symTab->finalCheck();

   if (d_stats.size() == 0)
        throw com::Exception("Empty model script, nothing to execute");

   setupClone();


   if (!d_xmlFile.isEmpty()) {
     pcrxml::Script s;
     s.ioStrategy = ioFieldStrategy().xmlType();
     s.scriptType= isDynamicModel() ? pcrxml::ScriptType::Dynamic :
                                      pcrxml::ScriptType::Static;
     if (!d_scriptFileName.isEmpty()) // cmd line otherwise
       s.scriptFileName =  d_scriptFileName.toString();
     if (isDynamicModel()) {
      /*
       * s.integerTimer        = new pcrxml::IntegerTimer();
       * s.integerTimer->start = d_timerStart;
       * s.integerTimer->end   = d_timerEnd;
       * s.integerTimer->step  = d_timerSlice;
       */
     }
     s.scriptData = new pcrxml::ScriptData();
     d_symTab->createXmlData(s.scriptData->data);
     s.write(d_xmlFile);
   }
}

const geo::RasterSpace& calc::Script::rasterSpace() const
{
    return ioFieldStrategy().rasterSpace();
}

/* will only return a valid Compressor after buildScript() is executed
 */
const calc::Compressor& calc::Script::compressor() const
{
    PRECOND(d_compressor);
    return *d_compressor;
}


bool calc::Script::inDynamic() const
{
    return false;
}

//! register that script contains at least one statement prefixed by report
/*!
 * \todo should go into the report table
 */
void calc::Script::setReportFound()
{
    d_aReportFound = true;
}

const calc::Report *calc::Script::reportDefault()const
{
    return d_reportTable.reportDefault();
}

//! add a report, error if already defined
/*
   \param r to add, deleted in case of error
 */
void calc::Script::addReport(const ReportDefinition *r)
{
  d_reportTable.insert(r);
}

//! find a report, that must exist
const calc::Report *calc::Script::findReport(const Symbol& u) const
{
    return d_reportTable.find(u);
}

/*!
 * \todo
 *   clientinterface/test13 is a hack
 */
bool calc::Script::allIsWritten() const
{
    // in a dynamic model only write if there
    //  is an explicit report
    if(isDynamicModel())
        return false;
    // otherwise if none is specified with a report
    // we do write all

    if (ioFieldStrategy().strategyType() == APP_IO_BANDMAP)
     return false;  // clientinterface/test13
    else
     return !d_aReportFound;
}

void calc::Script::print(calc::InfoScript& i)const
{
    printBlock(i);
}

//! print html file iff user gave -H flag
void calc::Script::htmlPrint() const
{
    if (d_htmlFile.isEmpty())
        return;

    calc::InfoScript si(d_htmlFile.toString());
    print(si);
}


//! detect format of \a par, check if valid
calc::FieldMapInputParameter* calc::Script::detectExternalFieldLeaf(const ParsPar &par)
{
    calc::FieldMapInputParameter *f;
    try {
      f=ioFieldStrategyMod().createFieldMapInputParameter(par);
    } catch (const com::Exception& excep) {
        par.symError(excep);
    }
    return f;
}

//! set the area map
void calc::Script::setAreaMap(const Symbol&  name)
{
    // we can not suffice with clone picked up
    // somewhere in the script, by now
    // the clone, MUST be set to the areamap or cli clone IF
    // AVAILABLE, since DebugMvAssignments needs a boolean

    PRECOND(!d_areaMap); // should be called exactly once per script
    UsePar pp(this, name);
    pp.setInputFilePath();
    d_areaMap = detectExternalFieldLeaf(pp);
}

//! make a non-user symbol
calc::Symbol calc::Script::generatedSymbol(
    const std::string& context,
    const std::string& name)
{
  PositionNone pn(context);
  return Symbol(this,name,&pn);
}

/*!
 * \todo
 *   if d_compression but clone has all true values do not install compressor
 */
void calc::Script::setupClone()
{
  // hard set a mask:
  // setAreaMap(generatedSymbol("--clone","mask.bil"));
  // d_compression=true;
  // PRINT_VAR(d_compression);

  // Do I need an explicit --clone/areamap setting
  bool needClone = debugMvAssignments() || zeroCompression() || d_compression;

  if (needClone
      || rasterSpace().nrRows() == 0) // no clone booted
  {
    if (!d_areaMap) { // we do not have an areamap, look for --clone
      if (!appClone) // Except 1
        throw com::Exception("no clone or area map specified");
      // pcrcalc/test82
      setAreaMap(generatedSymbol("--clone",appClone));
    }
  }

  // if Except 1 is not thrown then we do have a clone here
  ioFieldStrategyMod().setupClone();

  // First set as Null, in need for reading areaMap
  d_compressor = new NullCompressor(rasterSpace());

  if (needClone) {
     if (!isIn(d_areaMap->vs(),VS_B)) { //pcrcalc test248a
      d_areaMap->posError("area or clone map must be boolean when using -d or -m");
     }
     d_areaMap->restrictType().restrictSystem(VS_B,true);
     d_areaMap->goInScope();
     FieldHandle f = d_areaMap->value(0, true);
     d_areaMask = new unsigned char[f->nrValues()];
     std::memcpy(d_areaMask,f->srcValue(), f->nrValues());
  }

  if (d_compression) {
    delete d_compressor;
    d_compressor = new MaskCompressor(rasterSpace(),d_areaMask);
  }
}

//! find an expected input map
calc::FieldMapInputParameter* calc::Script::addExternalFieldLeaf(
        const ParsPar &par)
{
    calc::FieldMapInputParameter *p = detectExternalFieldLeaf(par);
    addSymbol(p);
    return p;
}

void calc::Script::removeOutputObject(
          const std::string& fileName) const
{
   ioFieldStrategy().removeOutputObject(fileName);
}

/*! create a map
    \exception com::Exception if the map can not be created
 */
calc::GridMap *calc::Script::createMap(const std::string& fileName, VS vs) const
{
 try {
  return ioFieldStrategy().createMap(fileName,vs);
 } catch( const com::FileError& f)  {
    throw com::Exception(f.messages());
 }
}

void calc::Script::checkClone(const std::string& mapFileName)
{
// only called from indextable initialization
 ioFieldStrategyMod().checkClone(mapFileName);
}

//! RunDirectory::inputFilePath()
std::string  calc::Script::inputFilePath(const std::string& fileName) const
{
  bool found;
  return d_runDirectory.inputFilePath(found,fileName);
}

//! RunDirectory::outputFilePath()
std::string calc::Script::outputFilePath(const std::string& fileName) const
{
  return d_runDirectory.outputFilePath(fileName);
}

//! return a new format appropriate stack reader
/*!
    callee must delete
 */
const calc::StackReader* calc::Script::createStackReader(const std::string& stackName)
{
  return ioFieldStrategyMod().createStackReader(d_runDirectory,stackName);
}

//! set the d_progressCallBack
void calc::Script::setProgressCallBack(ProgressCallBack *progressCallBack)
{
  PRECOND(d_progressCallBack); // is always set
  PRECOND(progressCallBack);   // is always set
  d_progressCallBack = progressCallBack;
}

//! update the d_progressCallBack
/*!
 * \a p is the pulse type: updateProgress() is called since we start
 *     the next loop or next statement.
 * \throws
 *   calc::QuitForProgressCallBack() if the callback function
 *   return non-zero
 */
void calc::Script::updateProgress(ProgressPulse p,int step)
{
  PRECOND(d_progressCallBack);

  if (p > d_progressCallBack->callAtPulse())
    return;

  ProgressInfo pi;
  if (step < 0)
     step = currentTimeStep();
  pi.inTimeStep =step;
  pi.nrTimeSteps=isDynamicModel() ? nrTimeSteps() : 0;
  if (d_progressCallBack->update(pi))
    throw calc::QuitForProgressCallBack();
}

void calc::Script::processFileOutputValue(
  double val)
{
  if (d_fileOutputStream) {
    *d_fileOutputStream << val << "\n";
     d_fileOutputStream->flush();
  }

  int ival(static_cast<int>(val));
  bool quit = updateExitVal(ival);


  if (quit) { // pcrcalc/test13b
    updateExitVal(static_cast<int>(currentTimeStep()));
    throw calc::QuitForExitOption();
  }
}

//! register value of fileoutput expr, check if model must be aborted
 /*! returns true if we may quit, false otherwise
  */
bool calc::Script::updateExitVal(double val)
{
  if (d_exitValueType == ALWAYS_0)
    return false;
  d_exitVal = static_cast<int>(val);
  if (d_exitValueType == EXIT_ON_0)
    return d_exitVal == 0;
  return false;
}

int calc::Script::exitVal() const
{
  return d_exitVal;
}

void calc::Script::setArcViewExtCheckData(std::vector<ArcViewExtCheckData>& r)
  const
{
  d_symTab->setArcViewExtCheckData(r);
}
