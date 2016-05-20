#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MODELBUILDER
#include "calc_modelbuilder.h"
#define INCLUDED_CALC_MODELBUILDER
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif
// Module headers.
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_EXECUTOR
#include "calc_executor.h"
#define INCLUDED_CALC_EXECUTOR
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif

/*!
  \file
  This file contains the implementation of the ModelBuilder class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC MODELBUILDER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MODELBUILDER MEMBERS
//------------------------------------------------------------------------------

calc::ModelBuilder::ModelBuilder():
  d_script(0),
  d_positionName(new std::string("??"))
{
  globalInit();
}

calc::ModelBuilder::~ModelBuilder()
{
  delete d_script;
  SetClone(0);
}

//! set name for \class calc::PositionName used to create symbols
void calc::ModelBuilder::setPositionName(const std::string& name)
{
  d_positionName.reset(new std::string(name));
}


//! set model
void calc::ModelBuilder::setModel(const std::string& model)
{
  PRECOND(!d_script); // FTTB call only once
  d_script=StringParser::createScript(model);
  d_script->setRteSettings(d_rtes);
}




void calc::ModelBuilder::setClone(const std::string& clone)
{
  SetClone(clone.c_str());
}


//! run the statements added
void calc::ModelBuilder::execute()
{
  d_script->analyzeAndResolve();

  Executor e(d_script->cfgCode(),
             d_script->rteSettings(),
             d_script->symbols());
  e.execAll();
}


void calc::ModelBuilder::setGlobalOption(const std::string& option)
{
  std::string withDash = "--"+option;
  PRECOND(ParseGlobalFlag(withDash.c_str()));
  ParseGlobalFlag(withDash.c_str());
}

void calc::ModelBuilder::setMVCompression(bool enable)
{
  d_rtes.setMVCompression(enable);
}
void calc::ModelBuilder::setCompile(bool enable)
{
  d_rtes.setCompile(enable);
}

// #ifndef INCLUDED_CALC_WRITEINFO
// #include "calc_writeinfo.h"
// #define INCLUDED_CALC_WRITEINFO
// #endif
// #ifndef INCLUDED_CALC_USEPAR
// #include "calc_usepar.h"
// #define INCLUDED_CALC_USEPAR
// #endif
//
// #ifndef INCLUDED_CALC_ASSIGNMENT
// #include "calc_assignment.h"
// #define INCLUDED_CALC_ASSIGNMENT
// #endif
// #ifndef INCLUDED_CALC_LOOKUPTABLEPARAMETER
// #include "calc_lookuptableparameter.h"
// #define INCLUDED_CALC_LOOKUPTABLEPARAMETER
// #endif
// #ifndef INCLUDED_CALC_RUNSETTINGS
// #include "calc_runsettings.h"
// #define INCLUDED_CALC_RUNSETTINGS
// #endif
//
// //! add a statement
//  *!
//  * \todo this method can not parse report keyword by definition
//  * should generate syntax error, not assertion failed:
//  * mb.addStatement("report ModelBuildertestMS.res = ModelBuildertestMS4 + 1");
//  */
// void calc::ModelBuilder::addStatement(const std::string& statement, bool write)
// {
//   PRECOND(!d_script); // FTTB call only once
//   d_script=StringParser::createScript(statement);
//   if (write)
//     d_script.setReportFound();
//   WriteInfo wi(&d_script,write,0,false);
// }
//
//
//
// void  calc::ModelBuilder::addLookupTable(
//   const std::string& name,
//   LookupTable       *table)
// {
//   std::vector<LookupTable *> val(1);
//   val[0]=table;
//   d_script.addSymbol(new LookupTableParameter(usePar(name),val));
// }
//
// //! add a statement of type \a par = \a expr
// void calc::ModelBuilder::addFieldAssignment(
//     const std::string& par,
//     ASTExpr*         expr,
//     bool write)
// {
//   if (write)
//     d_script.setReportFound();
//   WriteInfo wi(&d_script,write,0,false);
//
//   d_script.addStatement(
//      new Assignment(&d_script, wi,usePar(par) ,expr));
// }
//
// //! add an expression that yields a field
// calc::ASTExpr* calc::ModelBuilder::addASTExpr(const std::string& expr)
// {
// //  ParserInput pi(expr);
// //  Parser parser(pi.tokenBuffer());
// //  parser.initialize(d_script);
// //
// //  int retsignal;
// //  ASTExpr *e = parser.expr(&retsignal, &d_script);
// //  return e;
// }
//
// //! add a binding
// void calc::ModelBuilder::addBinding(
//     const std::string& left,
//     const std::string& right)
// {
//   d_script.addBinding(symbol(left), symbol(right));
// }
//
// //!  \sa  calc::Script::evaluateBindings()
// void calc::ModelBuilder::evaluateBindings()
// {
//       d_script.evaluateBindings();
// }
// calc::UsePar calc::ModelBuilder::usePar(const std::string& par)
// {
//   return UsePar(&d_script,symbol(par));
// }
//
// calc::Symbol calc::ModelBuilder::symbol(const std::string& name)
// {
//   PositionName pn(d_positionName);
//   return Symbol(&d_script,name,&pn);
// }
//

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



