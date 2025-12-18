#include "stddefx.h"
#include "calc_fieldmapinputparameter.h"
#include "calc_iofieldstrategy.h"
#include "calc_spatialimpl.h"
#include "calc_iscript.h"

//! ctor
calc::FieldMapInputParameter::FieldMapInputParameter(const ParsPar &par, bool constant, VS vs,
                                                     const std::vector<std::string> &vals,
                                                     const IoFieldStrategy &s)
    : FieldNewParameter(par, constant, true, vs, ST_SPATIAL), d_initVals(vals), d_ioFieldStrategy(s)
{
  POSTCOND(d_initVals.size() == nrElements());
}

//! assure values are accessible
void calc::FieldMapInputParameter::goInScope()
{
  for (size_t i = 0; i < nrElements(); i++) {
    if (d_value[i] == nullptr) {
      d_value[i] = new calc::SpatialImpl(
          *this, i, d_ioFieldStrategy.newInputMap(d_initVals[i], vs(), scriptConst().compressor()));
    }
  }
}
