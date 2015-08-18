#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_P5STACK
#include "calc_p5stack.h"
#define INCLUDED_CALC_P5STACK
#endif

// Library headers.

// PCRaster library headers.
// Module headers.

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_USEDEFANALYZER
#include "calc_usedefanalyzer.h" // setLastUse
#define INCLUDED_CALC_USEDEFANALYZER
#endif

/*!
  \file
  This file contains the implementation of the P5Stack class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC P5Stack MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF P5Stack MEMBERS
//------------------------------------------------------------------------------

calc::P5Stack::P5Stack()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::P5Stack::P5Stack(P5Stack const& rhs)

  : Base(rhs)

{
}
*/

void calc::P5Stack::init()
{
  d_as->transferAreaMap(new ASTPar("inp5s.map"));
  d_as->rebuildCFG();
  d_as->analyzeAndResolve();

  // redo setLastUse,  keep last use for some tests
  setLastUse(d_as->cfgCode(),d_keepLive);

  d_e.reset(
      new Executor(d_as->cfgCode(),
                   d_as->rteSettings(),
                   d_as->symbols()));
  d_e->execAllKeep();
}


calc::P5Stack::P5Stack(const std::string&  codeOrId):
  d_keepLive(true)
{
  d_as.reset(createFromIdOrStr(codeOrId));
  init();
}

calc::P5Stack::P5Stack(const std::string&  codeOrId,
                       bool                keepLastUse):
  d_keepLive(keepLastUse)
{
  d_as.reset(createFromIdOrStr(codeOrId));
  init();
}

calc::P5Stack::P5Stack(CompileTest  code):
  d_keepLive(true)
{
  d_as.reset(createFromIdOrStr(code.d_code));
  d_as->d_rteSettings.setCompile(true);
  init();
}



calc::P5Stack::~P5Stack()
{
  if (d_keepLive)
   d_e->d_rte.deleteAllValues();
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::P5Stack& calc::P5Stack::operator=(P5Stack const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

bool calc::P5Stack::contains(const std::string& name) const
{
  return d_e->runTimeEnv().dataTable().contains(name);
}

calc::Field *calc::P5Stack::popResult() const
{
  return d_e->popResult();
}

bool calc::P5Stack::equal(
      const Field* f,
      double allValues,
      VS vs,
      bool spatial)
{
  if (f->isSpatial() != spatial)
    return false;
  if (f->vs() != vs)
    return false;

  double v;
  if (!spatial) {
    f->getCell(v, 0);
    return v==allValues;
  }
  // else check spatial contents
  // 0 is MV
  if (f->getCell(v, 0))
    return false;
  PRECOND(f->nrValues() == 25);
  for(size_t i=1; i < 25; ++i) {
    if (!f->getCell(v, i))
      return false;
    if (v!=allValues)
      return false;
  }
  return true;
}

bool calc::P5Stack::equal(
      const std::string& name,
      double allValues,
      VS vs,
      bool spatial)
{
  return equal(fieldCast(name),allValues,vs,spatial);
}

//! get field value by name from data table
/*!
 * \post
 *   !v: dynamic_cast returned a valid pointer
 */
const calc::Field* calc::P5Stack::fieldCast(const std::string& name) const
{
   PRECOND(contains(name));
   const DataValue* dv(d_e->runTimeEnv().dataTable()[name]);
   POSTCOND(dv);
   const Field *f = dynamic_cast<const Field *>(dv);
   POSTCOND(f);
   return f;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



