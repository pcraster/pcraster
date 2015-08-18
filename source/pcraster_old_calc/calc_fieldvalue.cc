#include "stddefx.h"
#ifndef INCLUDED_CALC_FIELDVALUE
#include "calc_fieldvalue.h"
#define INCLUDED_CALC_FIELDVALUE
#endif

#ifndef INCLUDED_CALC_FIELDPARAMETER
#include "calc_fieldparameter.h"
#define INCLUDED_CALC_FIELDPARAMETER
#endif

calc::FieldValue::FieldValue(
  const FieldParameter& p,
  size_t index,
  Field *val):
 d_par(p),d_index(index),d_fw(d_par,d_index,p.vs()),d_val(new FieldHandle(val))
{}

calc::FieldValue::FieldValue(
  const FieldParameter& p,
  size_t index):
 d_par(p),d_index(index),d_fw(d_par,d_index,p.vs()),d_val(0)
{}

calc::FieldValue::~FieldValue()
{
  delete d_val;
}

const calc::Field *calc::FieldValue::value() const
{
  PRECOND(d_val);
  return d_val->get_rep();
}

//! Returns reference counted value of field
/*! If isLastUse is true, then the single existent reference
 *  is returned and its internal value is "deleted"
 *  \throws calc::Field::NotInitialized is there is no value
 */
calc::FieldHandle calc::FieldValue::value(bool isLastUse)
{
  if (! d_val)
    throw  calc::Field::NotInitialized();
  FieldHandle f = *d_val;
  if (isLastUse) {
    delete d_val;
    d_val= 0;
  }
  return f;
}

void calc::FieldValue::assign(
  FieldHandle f,
  const Position *assignPoint)
{
  delete d_val;
  d_val = new FieldHandle(f);
  if (d_par.writeHere(assignPoint))
    write();
}
