#include "stddefx.h"
#include "calc_modelbuilder.h"
#include "appargs.h"
#include "tokens.h"
#include "Parser.h"
#include "calc_parserinput.h"
#include "calc_writeinfo.h"
#include "calc_usepar.h"
#include "calc_assignment.h"
#include "calc_lookuptableparameter.h"
#include "calc_runsettings.h"
#include "calc_symbol.h"
#include "calc_positionname.h"
#include "calc_calc.h"

#include <memory>

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
  d_positionName(new std::string("??"))
{
  globalInit();
}

calc::ModelBuilder::~ModelBuilder()
{
  SetClone(nullptr);
}

//! set name for \class calc::PositionName used to create symbols
void calc::ModelBuilder::setPositionName(const std::string& name)
{
  d_positionName = std::make_shared<std::string>(name);
}

void calc::ModelBuilder::setMVCompression(bool enable)
{
  d_script.setMVCompression(enable);
}
void calc::ModelBuilder::set0Compression(bool enable)
{
  d_script.set0Compression(enable);
}

//! add a statement
/*!
 * \todo this method can not parse report keyword by definition
 * should generate syntax error, not assertion failed:
 * mb.addStatement("report ModelBuildertestMS.res = ModelBuildertestMS4 + 1");
 */
void calc::ModelBuilder::addStatement(const std::string& statement, bool write)
{
  ParserInput pi(statement);
  Parser parser(pi.tokenBuffer());
  parser.initialize(d_script);

  if (write)
    d_script.setReportFound();
  WriteInfo wi(&d_script,write,nullptr,false);
  int retsignal = 0;
  parser.assignment(&retsignal, &d_script,wi);
}

void calc::ModelBuilder::addStatement(Statement *stat)
{
  d_script.addStatement(stat);
}

//! add a statement of type \a par = \a expr
void calc::ModelBuilder::addFieldAssignment(
    const std::string& par,
    FieldExpr*         expr,
    bool write)
{
  if (write)
    d_script.setReportFound();
  WriteInfo wi(&d_script,write,nullptr,false);

  addStatement(
     new Assignment(&d_script, wi,usePar(par) ,expr));
}

//! add an expression that yields a field
/*!
 */
calc::FieldExpr* calc::ModelBuilder::addFieldExpr(const std::string& expr)
{
  ParserInput pi(expr);
  Parser parser(pi.tokenBuffer());
  parser.initialize(d_script);

  int retsignal = 0;
  FieldExpr *e = parser.expr(&retsignal, &d_script);
  return e;
}

//! add a series of bindings from an ascii file \a bindingFile
calc::RunSettings calc::ModelBuilder::parseExternalBindings(const com::PathName& bindingFile)
{
  ParserInput pi(bindingFile);
  Parser parser(pi.tokenBuffer());
  parser.initialize(d_script);

  RunSettings m;

  int retsignal = 0;
  parser.externalBindings(&retsignal, m);
  return m;
}


//! add a binding
void calc::ModelBuilder::addBinding(
    const std::string& left,
    const std::string& right)
{
  d_script.addBinding(symbol(left), symbol(right));
}

/*!
 * \sa  calc::Script::evaluateBindings()
 */
void calc::ModelBuilder::evaluateBindings()
{
      d_script.evaluateBindings();
}

void calc::ModelBuilder::setClone(const std::string& clone)
{
  SetClone(clone.c_str());
}


//! run the statements added
void calc::ModelBuilder::execute()
{
  d_script.buildScript();
  d_script.run();
}

void  calc::ModelBuilder::addLookupTable(
  const std::string& name,
  LookupTable       *table)
{
  std::vector<LookupTable *> val(1);
  val[0]=table;
  d_script.addSymbol(new LookupTableParameter(usePar(name),val));
}

calc::UsePar calc::ModelBuilder::usePar(const std::string& par)
{
  return UsePar(&d_script,symbol(par));
}

calc::Symbol calc::ModelBuilder::symbol(const std::string& name)
{
  PositionName pn(d_positionName);
  return Symbol(&d_script,name,&pn);
}

void calc::ModelBuilder::setGlobalOption(const std::string& option)
{
  std::string withDash = "--"+option;
  PRECOND(ParseGlobalFlag(withDash.c_str()));
  ParseGlobalFlag(withDash.c_str());
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



