#include "stddefx.h"

#ifndef INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#include "calc_fieldmapinputparameter.h"
#define INCLUDED_CALC_FIELDMAPINPUTPARAMETER
#endif

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif

#ifndef INCLUDED_CALC_SPATIALIMPL
#include "calc_spatialimpl.h"
#define INCLUDED_CALC_SPATIALIMPL
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

//! ctor
calc::FieldMapInputParameter::FieldMapInputParameter(
    const ParsPar& par,
    bool constant,
    VS   vs,
    const std::vector<std::string>& vals,
    const IoFieldStrategy& s):
  FieldNewParameter(par,constant,true,vs,ST_SPATIAL),
  d_initVals(vals),
  d_ioFieldStrategy(s)
{
  POSTCOND(d_initVals.size() == nrElements());
}

//! assure values are accessible
void calc::FieldMapInputParameter::goInScope()
{
  for(size_t i=0; i < nrElements(); i++)
   if (!d_value[i])
    d_value[i] = new calc::SpatialImpl(
        *this,
        i,
        d_ioFieldStrategy.newInputMap(
               d_initVals[i],vs(),scriptConst().compressor())
            );
}
