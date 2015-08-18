#include "stddefx.h"

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

calc::FieldStack::FieldStack()
{
}

calc::FieldStack::~FieldStack()
{
  FieldsPopped popAll(*this,d_stack.size());
  POSTCOND(d_stack.empty());
}

//! pop existing value that is (re-)used as destination
/*!
 * newVs denote the new vs possible
 * this can happen since we re-use on cellRepr not vs,
 * see pcrcalc/test6[56]
 */
calc::FieldHandle calc::FieldStack::popDest(VS newVs)
{
  PRECOND(!d_stack.empty());
  FieldHandle v = d_stack.top();
  d_stack.pop();
  if (! v.isOnlyHandle())
    v = v->copy();
  v->resetVs(newVs);
  return v;
}

//! returns a field that can only be read
calc::FieldHandle calc::FieldStack::popReadOnly()
{
  PRECOND(!d_stack.empty());
  FieldHandle v = d_stack.top();
  d_stack.pop();
  return v;
}

void calc::FieldStack::push(FieldHandle v)
{
  d_stack.push(v);
}

calc::FieldsPopped::FieldsPopped(FieldStack& stack, size_t nr)
{
  for (size_t i =0; i < nr ; i++)
    push_back(stack.popReadOnly());
}
