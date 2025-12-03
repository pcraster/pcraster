#include "stddefx.h"
#include "calc_fieldnewparameter.h"
#include "calc_iscript.h"
#include "calc_iofieldstrategy.h"
#include "calc_spatialimpl.h"
#include "calc_nonspatialimpl.h"
#include "calc_nonspatialtssimpl.h"
#include "calc_field.h"

//! ctor
calc::FieldNewParameter::FieldNewParameter(const calc::ParsPar &par, bool constant, bool input, VS vs,
                                           ST st)
    : calc::FieldParameter(par, constant, input, vs, st), d_value(nrElements(), nullptr)
{
}

//! dtor
calc::FieldNewParameter::~FieldNewParameter()
{
  for (auto &i : d_value) {
    delete i;
  }
}

//! throw com::Exception if name validation fails
void calc::FieldNewParameter::moreValidation(const std::string &fileName) const
{
  if (!reportedInDynamic()) {
    // check on ESRI grid restrictions when parameter is
    //  written as a single grid, not a tss or stack, happens
    //  only in initial section
    scriptConst().ioFieldStrategy().validateFileName(fileName);
  }
  SubParameter::moreValidation(fileName);
}

//! create parameters now they are in scope
void calc::FieldNewParameter::goInScope()
{
  for (size_t i = 0; i < d_value.size(); i++) {
    if (fieldType().spatial()) {
      d_value[i] = new SpatialImpl(*this, i);
    } else {
      if (reportedInDynamic()) {
        d_value[i] = new NonSpatialTssImpl(*this, i);
      } else {
        d_value[i] = new NonSpatialImpl(*this, i);
      }
    }
  }
}

//! return the parameter value
calc::Handle<calc::Field> calc::FieldNewParameter::value(size_t index, bool lastUse)
{
  PRECOND(index < nrElements());
  return d_value[index]->value(lastUse);
}

void calc::FieldNewParameter::assign(const Handle<Field> &f, size_t index, const Position *assignPoint)
{
  PRECOND(index < nrElements());
  d_value[index]->assign(f, assignPoint);
}
