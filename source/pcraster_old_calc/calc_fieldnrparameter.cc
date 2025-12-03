#include "stddefx.h"
#include "calc_fieldnrparameter.h"
#include "calc_nonspatial.h"
#include "calc_nonspatialimpl.h"

calc::FieldNrParameter::FieldNrParameter(const ParsPar &par, bool constant,
                                         const std::vector<double> &vals, VS vs)
    : FieldParameter(par, constant, false, vs, ST_NONSPATIAL), d_vals(vals.size(), nullptr),
      d_initVals(vals)
{
}

//! single constant value as apparent in binding
calc::FieldNrParameter::FieldNrParameter(const ParsPar &par, double value, VS vs)
    : FieldParameter(par, true, false, vs, ST_NONSPATIAL)
{
  d_vals.push_back(nullptr);
  d_initVals.push_back(value);
}

calc::FieldNrParameter::~FieldNrParameter()
{
  for (auto &d_val : d_vals) {
    delete d_val;
  }
}

void calc::FieldNrParameter::goInScope()
{
  for (size_t i = 0; i < d_initVals.size(); i++) {
    d_vals[i] = new NonSpatialImpl(*this, i, new NonSpatial(vs(), d_initVals[i]));
  }
}

calc::FieldHandle calc::FieldNrParameter::value(size_t index, bool lastUse)
{
  if (!(index < d_vals.size())) {
    POSTCOND(index < d_vals.size());
  }
  return d_vals[index]->value(lastUse);
}

//! return the initial value for the \a index element
double calc::FieldNrParameter::initValue(size_t index) const
{
  PRECOND(index < d_vals.size());
  return d_initVals[index];
}

//! hack to get value for export to XML, and TimerValue
/*!
 * \todo
 *   get rid of this hack
 */
double calc::FieldNrParameter::initialValue() const
{
  return initValue(0);
}
