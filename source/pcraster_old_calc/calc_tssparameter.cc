#include "stddefx.h"

#ifndef INCLUDED_CALC_TSSPARAMETER
#include "calc_tssparameter.h"
#define INCLUDED_CALC_TSSPARAMETER
#endif

#ifndef INCLUDED_CALC_NEWXMLDATASUBTYPE
#include "calc_newxmldatasubtype.h"
#define INCLUDED_CALC_NEWXMLDATASUBTYPE
#endif

//! always returns VS_TSS
VS calc::TssParameter::symbolType() const
{
  return VS_TSS;
}

calc::TssParameter::TssParameter(
    const calc::ParsPar& par,
    bool constant,
    bool input):
  calc::SubParameter(par,constant,input)
{
}

void calc::TssParameter::setDataSubType(pcrxml::Data *d) const
{
  d->timeSeries = newDataSubType<pcrxml::TimeSeries>(vs());
}
