#include "stddefx.h"

#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif

#ifndef INCLUDED_CALC_STACKREADER
#include "calc_stackreader.h"
#define INCLUDED_CALC_STACKREADER
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h" // FieldHandle of this type
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

calc::StackInput::StackInput(
    const Element&       pos,
    const BindedSymbol& stackName,
    bool  sparse):
  FieldExpr(pos),
  d_reader(0),
  d_itemToLoad(scriptConst().nrTimeSteps()+1),
  d_type(VS_FIELD,ST_SPATIAL)
{
  try {
   d_reader = script().createStackReader(stackName.externalName());

   // check if for each timestep a map is there with identical VS
   // and if they ALL have the same format supported by d_reader; esrigrid/test26
   VS vs = VS_FIELD;
   d_itemToLoad[0]=0; // dummy for fall back to previous
   size_t firstExisting=0;
   for(size_t i=1; i < d_itemToLoad.size(); i++) {
      if (sparse && !d_reader->itemExists(i)) {
        // fall back to previous
        d_itemToLoad[i]=d_itemToLoad[i-1];
        continue; // do not check non existing item
      } else {
        d_itemToLoad[i]=i;
      }

     // item i exist, at this point in code
     if (!firstExisting)
      firstExisting=i;

     try {
      VS newVs = d_reader->checkItem(i,vs);
      if (!isIn(newVs,vs)) { // pcrcalc/test250(ac)
       throw com::Exception(
        " does not have the same data type as first element ("+toString(vs)+")");
      }
      vs = newVs;
     } catch ( com::Exception& msg ) {
       // rewrote construction of posError param. win32/release
       //  gives strange error
       std::string s(d_reader->itemName(i));
       s+=": ";
       s+=msg.messages();
        // pcrcalc/test344(a)
        stackName.posError(" on checking map-stack "+stackName.qName()
            +"\n  element "+s);
     }
   } // eofor
   // at least 1 element needed
   if (!firstExisting) {
        // pcrcalc/test376
        stackName.posError(" on checking map-stack "+stackName.qName()
            +"\n not a single map found");
   }
   // if 1st element is missing read first existing one
   for(size_t i=1; i < firstExisting; i++)
     d_itemToLoad[i]=firstExisting;
   restrictType().restrictSystem(vs,spatial());
  } catch (...) {
    delete d_reader;
    throw;
  }
}

calc::StackInput::~StackInput()
{
  delete d_reader;
}

void calc::StackInput::buildTypesRecursive(VS resultVsSet)
{
  POSTCOND(resultVsSet != VS_UNKNOWN);
  (void) resultVsSet; // shut up compiler
}

calc::FieldType& calc::StackInput::restrictType()
{
  return d_type;
}

void calc::StackInput::prepareExecution()
{
}

void calc::StackInput::skipExecution()
{
}

const calc::FieldType& calc::StackInput::fieldType() const
{
  return d_type;
}

/*!
 * \todo
 *   optimize by caching map if known to be used again, and return a copy
 *   except in last use case.
 */
void calc::StackInput::execute(FieldStack& stack)
{
  try {
   PRECOND(scriptConst().currentTimeStep() < d_itemToLoad.size());
   size_t itemToRead=d_itemToLoad[scriptConst().currentTimeStep()];
   stack.push(FieldHandle(
    d_reader->read(itemToRead, vs(),scriptConst().compressor())));
  } catch(const com::FileError& e) {
    runtimeError(e.messages());
  }
}

void calc::StackInput::print(InfoScript &si) const
{
   si.stream() << "timeinput( ";
// si.parTag(stackName.name());
   si.stream() << " ) ";
}
