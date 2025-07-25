#include "stddefx.h"
#include "calc_tssparameter.h"
#include "calc_newxmldatasubtype.h"

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
