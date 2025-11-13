#include "stddefx.h"
#include "calc_fieldvalue.h"
#include "calc_fieldparameter.h"

calc::FieldValue::FieldValue(
  const FieldParameter& p,
  size_t index,
  Field *val):
 d_par(p),d_index(index),d_fw(d_par,d_index,p.vs()),d_val(new FieldHandle(val))
{}

calc::FieldValue::FieldValue(
  const FieldParameter& p,
  size_t index):
 d_par(p),d_index(index),d_fw(d_par,d_index,p.vs()),d_val(nullptr)
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
  FieldHandle const f = *d_val;
  if (isLastUse) {
    delete d_val;
    d_val= nullptr;
  }
  return f;
}

void calc::FieldValue::assign(
  const FieldHandle& f,
  const Position *assignPoint)
{
  delete d_val;
  d_val = new FieldHandle(f);
  if (d_par.writeHere(assignPoint))
    write();
}
