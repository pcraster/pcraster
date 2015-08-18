#include "stddefx.h"

#ifndef INCLUDED_CALC_TSSINPUTPARAMETER
#include "calc_tssinputparameter.h"
#define INCLUDED_CALC_TSSINPUTPARAMETER
#endif

#ifndef INCLUDED_CALC_TIMETABLE
#include "calc_timetable.h"
#define INCLUDED_CALC_TIMETABLE
#endif

calc::TssInputParameter::TssInputParameter(
    const calc::ParsPar& par,
    bool constant,
       const std::vector<calc::TimeTable *>& val) :
  calc::TssParameter(par,constant,true), d_vals(val)
{
  POSTCOND(d_vals.size() == nrElements());
}

calc::TssInputParameter::~TssInputParameter()
{
  for(size_t i=0; i< d_vals.size(); i++)
    delete d_vals[i];
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
