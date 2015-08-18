#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif

// Library headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"  // appClone
#define INCLUDED_APPARGS
#endif
// Module headers.
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"  // appClone
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_CODE
#include "calc_code.h"
#define INCLUDED_CALC_CODE
#endif
#ifndef INCLUDED_CALC_SYMEXCEPTION
#include "calc_symexception.h"
#define INCLUDED_CALC_SYMEXCEPTION
#endif
#ifndef INCLUDED_CALC_USEDEFANALYZER
#include "calc_usedefanalyzer.h"
#define INCLUDED_CALC_USEDEFANALYZER
#endif
#ifndef INCLUDED_CALC_BUILDTYPESVISITOR
#include "calc_buildtypesvisitor.h"
#define INCLUDED_CALC_BUILDTYPESVISITOR
#endif
#ifndef INCLUDED_CALC_RUNSETTINGS
#include "calc_runsettings.h"
#define INCLUDED_CALC_RUNSETTINGS
#endif
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
#ifndef INCLUDED_CALC_REPORTVISITOR
#include "calc_reportvisitor.h"
#define INCLUDED_CALC_REPORTVISITOR
#endif
#ifndef INCLUDED_CALC_CFGCREATOR
#include "calc_cfgcreator.h"
#define INCLUDED_CALC_CFGCREATOR
#endif
#ifndef INCLUDED_CALC_CFGNODE
#include "calc_cfgnode.h"
#define INCLUDED_CALC_CFGNODE
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCKDLL
#include "calc_pointcodeblockdll.h"
#define INCLUDED_CALC_POINTCODEBLOCKDLL
#endif
#ifndef INCLUDED_CALC_INSERTPOINTCODEBLOCKS
#include "calc_insertpointcodeblocks.h"
#define INCLUDED_CALC_INSERTPOINTCODEBLOCKS
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif

#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_TIMETABLE
#include "calc_timetable.h"
#define INCLUDED_CALC_TIMETABLE
#endif

/*!
  \file
  This file contains the implementation of the ASTScript class.
*/



//------------------------------------------------------------------------------

namespace calc {
  namespace Private {
    struct Check : public boost::noncopyable {
     const ASTSymbolTable& d_syms;
     Check(const ASTSymbolTable& syms):d_syms(syms) {}

     void notANumber(ASTId *id,
                     const std::string& name)
     {
         std::ostringstream os;
         os << id->qName() <<" is not a whole positive number (in "<<
               name << " definition)";
         id->posError(os);
     }

     // check id being a number of an id known as constant
     size_t operator()(
         ASTId *id,
         const std::string& name)
     {
       PRECOND(id);
       const ASTNumber *n=dynamic_cast<const ASTNumber *>(id);
       double value;
       if(n) {
         value=n->value();
       } else {
        PRECOND(d_syms.contains(id->name()));
        const ASTSymbolInfo&  a(d_syms[id->name()]);
        if(!a.isConstant()) // pcrcalc8b
          notANumber(id,name);
        value=a.constantValue();
       }
       if ( (!isIn(VS_N,id->returnDataType().vs())) || value < 0) {
          // pcrcalc7a
          notANumber(id,name);
       }
       return static_cast<size_t>(value);
     }
   };
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTSCRIPT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTSCRIPT MEMBERS
//------------------------------------------------------------------------------

calc::ASTScript::ASTScript():
  d_code(0),
  d_cfgCode(0),
  d_cfgBody(0),
  d_areaMap(0),
  d_externalTimer(0),
  d_timerStartOrTss(0),
  d_timerEnd(0),
  d_timerStep(0),
  d_containsDynamicSection(false),
  d_hasStatementWithReportKeyword(false),
  d_pointCodeBlockDll(0),
  d_reportOnlyForXMLScriptOutput(false)
{
  if (appClone)
    setAreaMapFromString(appClone,"--clone");
}

void calc::ASTScript::setAreaMapFromString(
  std::string const& value,
  std::string const& positionText)
{
    delete d_areaMap;
    d_areaMap= new ASTPar(value);
    PositionName pn(positionText);
    d_areaMap->setPosition(&pn);
}

calc::ASTScript::~ASTScript()
{
  delete d_areaMap;
  delete d_externalTimer;
  delete d_timerStartOrTss;
  delete d_timerEnd;
  delete d_timerStep;
  delete d_code;

  delete d_cfgCode;
  delete d_cfgBody;

  delete d_pointCodeBlockDll;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::ASTScript& calc::ASTScript::operator=(const ASTScript& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::ASTScript::ASTScript(const ASTScript& rhs):
  Base(rhs)
{
}
*/

/*! \brief transfer code, code may contain initial and dynamic section
 */
void calc::ASTScript::transferCode(ASTNode *code) {
  delete d_code;
  d_code=new Code(code);

  rebuildCFG();
}

void calc::ASTScript::rebuildCFG() {
  // TODO not very efficient doing it twice
  delete d_cfgCode;
  d_cfgCode = createCFG(d_code);
  delete d_cfgBody;

  std::vector<ASTNode *> body;
  body.push_back(d_areaMap);
  body.push_back(d_timerStartOrTss);
  body.push_back(d_timerEnd);
  body.push_back(d_timerStep);
  body.push_back(d_code);

  d_cfgBody = createCFG(body);

}

void calc::ASTScript::buildTypesFullClosure()
{
  BuildTypesVisitor btv(d_cfgBody);
  btv.init(d_symbols);

  size_t nrChanges=btv.nrChanges();
  do {
   nrChanges=btv.nrChanges();
   btv.visit();
   // Let's be reasonable about this
   PRECOND(btv.nrChanges() < 100000);
  } while(nrChanges != btv.nrChanges());

  // updated symbols
  d_symbols = btv.table();

  d_containsDynamicSection=btv.containsDynamicSection();
  d_hasStatementWithReportKeyword=btv.hasStatementWithReportKeyword();

}

/*
 * \brief attach info from interface and binding to d_symbols
 *        (as discovered in the BuildTypesVisitor).
 *
 * Note that applying the interface may restrict the DataType of d_symbols.
 */
void calc::ASTScript::applyInterface()
{
  EffectiveBindings bt(d_bindings);

  // 1) external binding -> bindings
  if (!d_rteSettings.externalBindingFile().isEmpty()) {
   RunSettings rs(d_rteSettings.externalBindingFile());
   bt.overwrite(rs);
  }

  // 2) apply bindings
  // make set of syms defined in interface
  std::set<std::string> interfaceSyms;
  for(size_t i=0; i<d_interface.size(); ++i)
    interfaceSyms.insert(d_interface[i].name());
  bt.applyToSymbols(d_symbols,interfaceSyms);

  // 3) apply interface
  for(size_t i=0; i<d_interface.size(); ++i) {
    ASTSymbolTable::iterator s= d_symbols.find(d_interface[i].name());
    if (s!=d_symbols.end())
      s->second.setInfo(d_interface[i]);
  }
}

/*! rewrite the AST for compiled code blocks if compile flag is set
 * \todo
 *   disabled and not working in tests at this moment
 */
void calc::ASTScript::compile()
{
  if (!d_rteSettings.compile())
    return;

 std::vector<PointCodeBlock *> l=
   insertPointCodeBlocks(d_symbols,d_code,d_cfgCode);
 d_pointCodeBlockDll= new PointCodeBlockDll(l);

 // AST is changed by insertPointCodeBlocks
 rebuildCFG();
 setLastUse(d_cfgCode);
}

/*
 * //! check and build up new symbol table using the xmlConfigure context
 * void calc::ASTScript::analyzeWithConfiguration(
 *     const std::string& xmlConfigure)
 * {
 *   XMLReflection xr(xmlConfigure);
 * 
 *   // d_rteSettings.resetIOStrategy(xr.ioStrategy());
 *   // POSTCOND(xr.ioStrategy()==pcrxml::IOStrategyType::IOMemory);
 * 
 *   // start with info from script itself
 *   analyzeNoContext();
 * 
 *   //  restrict d_symbols in types from xmlConfigure
 *   d_rteSettings.configureSymbols(d_symbols, xr);
 * }
 */

void calc::ASTScript::setSettingsFromXML(pcrxml::Script const& script)
{
  std::string areaMapRef =
    d_rteSettings.setSettingsFromXML(script);
  if (! areaMapRef.empty()) {
    setAreaMapFromString(areaMapRef,"<xmlApi>");
  }
}

void calc::ASTScript::callWithClashRewrite(
    void (ASTScript::*f)() )
{
 try {
      try {
          (this->*f)();
      } catch( const DataTypeClash& ) {
       // transform DataTypeClash into SymException
       //  see for example pcrcalc372a
       buildTypesFullClosure();
       POSTCOND(FALSE); // never here
      }
 } catch (const SymException& s) {
     d_symbols.throwSym(s);
 }
}

//! check and build up new symbol table without any context
/*!
 * no context means:
 * - reset d_symbols to empty at start.
 * - no resolve() to look for input data.
 * .
 *
 * \todo (*) since IOMemory is not yet correct for mapstacks
 *        this is not completely true, only the default will do
 */
void calc::ASTScript::analyzeNoContextUnChecked()
{
  // BuildTypesVisitor with empty table
  d_symbols = ASTSymbolTable();
  buildTypesFullClosure();

  setLastUse(d_cfgCode);

  // compute ioTypes
  typedef std::map<std::string,IOType> CodeTypes;
  CodeTypes codeTypes=ioTypes(d_cfgCode);
  for (CodeTypes::const_iterator i=codeTypes.begin();
       i!=codeTypes.end(); ++i) {
    ASTSymbolTable::iterator s=d_symbols.find(i->first);
    PRECOND(s!=d_symbols.end());
    s->second.setIoType(i->second);
  }

  applyInterface();

  // redo with type information found in applyInterface
  buildTypesFullClosure();


  if (d_rteSettings.useDiskStorage())
   d_symbols.checkDifferentExternalNames();
}


void calc::ASTScript::resolve() {
  callWithClashRewrite(&ASTScript::resolveUnChecked);
}
void calc::ASTScript::analyzeNoContext() {
  callWithClashRewrite(&ASTScript::analyzeNoContextUnChecked);
}

/*! resolve inputs and recheck types of input with AST and symbol info
 */
void calc::ASTScript::resolveUnChecked() {

  std::string areaMap;
  if (d_areaMap)
    areaMap=d_areaMap->name();

  // possible first time to see if it IS reported
  // NOTE never call setReport for the first time
  //      before setDefinition() is done
  setReports();

  Timer t(timer());

  d_rteSettings.resolve(d_symbols, areaMap, t);

  buildTypesFullClosure();
  // possible second or later time, now we know the timer end
  // that might came up from a read timeseries in resolve()
  setReports();

  if (containsDynamicSection() && !t.dynamic())
    d_code->posError("There is a dynamic section but no timer section");
}

/* \brief analyzeNoContext() followed by resolve()
 * Full configuration and checking of AST and symbols
 */
void calc::ASTScript::analyzeAndResolve()
{
 analyzeNoContext();
 resolve();
}


/*! update symbols for report info, what is written and what not
 */
void calc::ASTScript::setReports()
{
 Timer t = timer();
 if (!t.lastInt())
     t.setLastInt(1);

 d_reports.update(t);

 bool reportLastAssOfEverySymbol =
    !d_containsDynamicSection &&
     d_interface.empty() &&
    !d_hasStatementWithReportKeyword;
 if (d_reportOnlyForXMLScriptOutput)
   reportLastAssOfEverySymbol=true;

 ReportVisitor rv(reportLastAssOfEverySymbol, d_reports,t);
 if (d_code)
    d_code->accept(rv);

 ReportPars rps(rv.reportPars());
 for(ReportPars::const_iterator rp=rps.begin();rp!=rps.end();++rp) {
   ASTSymbolTable::iterator s= d_symbols.find(rp->first);
   POSTCOND(s != d_symbols.end());
   ReportPar const& rpp(rp->second);
   s->second.setReport(rpp.d_par,rpp.d_report,rpp.d_inDynamic,
                       d_reportOnlyForXMLScriptOutput);
 }
}

//! set value of d_reportOnlyForXMLScriptOutput
void calc::ASTScript::setReportOnlyForXMLScriptOutput(bool reportOnlyForXMLScriptOutput)
{
  d_reportOnlyForXMLScriptOutput=reportOnlyForXMLScriptOutput;
}


//! get value of cfgCode
calc::CFGNode*  calc::ASTScript::cfgCode() const
{
  return d_cfgCode;
}

//! get value of cfgCode
calc::ASTNode*  calc::ASTScript::astCode() const
{
  return d_code;
}


void calc::ASTScript::transferAreaMap(ASTId *areaMap)
{
  delete d_areaMap;
  d_areaMap=areaMap;
}

void calc::ASTScript::transferTimerSection(ASTId* timerStartOrTss,
                                           ASTId* timerEnd,
                                           ASTId* timerStep)
{
  PRECOND(!d_timerStartOrTss); // only called once
  d_timerStartOrTss=timerStartOrTss;
  d_timerEnd       =timerEnd;
  d_timerStep      =timerStep;
}

//! get value of rteSettings
const calc::RunTimeEnvSettings& calc::ASTScript::rteSettings() const
{
  return d_rteSettings;
}

void  calc::ASTScript::setRteSettings(RunTimeEnvSettings const& rteSettings)
{
  d_rteSettings=rteSettings;
}

//! get value of bindings
const calc::BindingTable& calc::ASTScript::bindings() const
{
  return d_bindings;
}

//! get value of reports
const calc::ReportTable& calc::ASTScript::reports() const
{
  return d_reports;
}

//! get const symbols
const calc::ASTSymbolTable& calc::ASTScript::symbols() const
{
  return d_symbols;
}
//
//! get value of containsDynamicSection
bool calc::ASTScript::containsDynamicSection() const
{
  return d_containsDynamicSection;
}


//! get modifiable symbols
calc::ASTSymbolTable& calc::ASTScript::symbols()
{
  return d_symbols;
}

void calc::ASTScript::transferOneBinding(ASTNode* bindingDefinition)
{
    d_bindings.transferPushBack(bindingDefinition);
}

void calc::ASTScript::addReportDefinition(Report const& reportDef)
{
    d_reports.add(reportDef);
}
void calc::ASTScript::addInterfaceDefinition(ASTDefinition const& astDef)
{
    d_interface.push_back(astDef);
}

//! set value of externalTimer
void calc::ASTScript::setExternalTimer(Timer const& externalTimer)
{
  delete d_externalTimer;
  d_externalTimer= new Timer(externalTimer);
}

calc::Timer calc::ASTScript::timer() const
{
 Timer timer;

 if (d_externalTimer)
    return *d_externalTimer;

 if (!d_timerStartOrTss)
   return timer; // no timer section, static model

 // else analyze d_timerStartOrTss

 PRECOND(!timer.lastInt());
 if (!d_timerStartOrTss->isNumber()) {
   const ASTSymbolInfo&  a(d_symbols[d_timerStartOrTss->name()]);
   if (!a.isConstant()) { // it is a (file)-symbol
     try {
       TimeTable tss(a.externalName(),VS_S,1);
       timer.setStartInt(1);
       timer.setLastInt(tss.nrTimeSteps());
      } catch(const com::Exception& e) {
        // pcrcalc364
        a.throwAtFirst(e); // happens to be first ?
      }
  }
 }

 // if not set by tss
 if (!timer.startInt()) {

   Private::Check c(d_symbols);
   timer.setStartInt(c(d_timerStartOrTss,"start time"));
   timer.setLastInt(c(d_timerEnd,"end time"));
   size_t step  = c(d_timerStep,"time step");


   if ( timer.startInt() == 0) { // pcrcalc6
     d_timerStartOrTss->posError("start time must be > 0 ");
   }
   if (step != 1) { // pcrcalc7
    std::ostringstream os;
    os<<"current limitation: time step must be 1 (not "<<step<<")";
    d_timerStep->posError(os);
   }
   if (timer.startInt() > timer.lastInt()) { // pcrcalc8
    std::ostringstream os;
    os<<"Start time ("<<timer.startInt()<<") is greater than end time ("
      << timer.lastInt() <<")";
    d_timerStartOrTss->posError(os);
   }
 }
 return timer;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------





//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



