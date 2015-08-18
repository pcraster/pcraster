#include "stddefx.h"

#ifndef INCLUDED_CALC_RUNTIMESTACK
#include "calc_runtimestack.h"
#define INCLUDED_CALC_RUNTIMESTACK
#endif

#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif

calc::RunTimeStack::RunTimeStack()
{
}

calc::RunTimeStack::~RunTimeStack()
{
#ifdef DEBUG_DEVELOP
   // in normal cases the stack should be empty
   // but not always when an exception is unwinded
   if (!std::uncaught_exception())
   {
      POSTCOND(d_stack.empty());
   }
#endif
   clean();
}

void calc::RunTimeStack::clean()
{
  while(!d_stack.empty()) {
    // OK
    deleteFromPcrme(d_stack.top());
    d_stack.pop();
    // WRONG, must supress load() in cleaning
    // up stack in exceptional case
    // WRONG: DataValue *dv=pop();
    // WRONG: deleteFromPcrme(dv);
  }
}

//! returns a loaded DataValue
/*!
 * If the stack top contains a command to read
 * something external (StackedValue/DiskWrittenField: a reference to a file map)
 * then this command is executed to load the value.
 * and pop returns the DataValue loaded.
 *
 * Who owns the result and can it be modified is managed
 * by the results's DataValue::readOnlyReference() and
 * DataValue::pcrmeManaged() settings.
 *
 * \todo
 *   LEAK mark2 user model crashed on deleting the second, the 
 *   DataTable possible holds references, this is a MESS!
 */
calc::DataValue* calc::RunTimeStack::pop()
{
  PRECOND(!d_stack.empty());
  DataValue *possibleProxy = d_stack.top();
  PRECOND(possibleProxy);
  d_stack.pop();
  DataValue *dv = possibleProxy->load();
  if (possibleProxy != dv) {
      // possibleProxy is only a proxy class
      // not the DataValue itself
      delete possibleProxy;
  }
  // again: StackedValue->DiskWrittenField->Field
  possibleProxy=dv;
  dv = possibleProxy->load();
  if (possibleProxy != dv) {
      ;// LEAK delete possibleProxy;
  }

  return dv;
}

void calc::RunTimeStack::push(DataValue* v)
{
  d_stack.push(v);
}

size_t calc::RunTimeStack::size() const
{
  return d_stack.size();
}
