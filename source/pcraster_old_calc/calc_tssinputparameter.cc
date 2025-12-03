#include "stddefx.h"
#include "calc_tssinputparameter.h"
#include "calc_timetable.h"

calc::TssInputParameter::TssInputParameter(const calc::ParsPar &par, bool constant,
                                           const std::vector<calc::TimeTable *> &val)
    : calc::TssParameter(par, constant, true), d_vals(val)
{
  POSTCOND(d_vals.size() == nrElements());
}

calc::TssInputParameter::~TssInputParameter()
{
  for (auto &d_val : d_vals) {
    delete d_val;
  }
}

calc::TimeTable *calc::TssInputParameter::value(size_t i)
{
  PRECOND(i < nrElements());
  return d_vals[i];
}

VS calc::TssInputParameter::vs() const
{
  return d_vals[0]->vs();
}
