#include "stddefx.h"

#ifndef INCLUDED_CALC_FIELDNEWPARAMETER
#include "calc_fieldnewparameter.h"
#define INCLUDED_CALC_FIELDNEWPARAMETER
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif

#ifndef INCLUDED_CALC_SPATIALIMPL
#include "calc_spatialimpl.h"
#define INCLUDED_CALC_SPATIALIMPL
#endif

#ifndef INCLUDED_CALC_NONSPATIALIMPL
#include "calc_nonspatialimpl.h"
#define INCLUDED_CALC_NONSPATIALIMPL
#endif

#ifndef INCLUDED_CALC_NONSPATIALTSSIMPL
#include "calc_nonspatialtssimpl.h"
#define INCLUDED_CALC_NONSPATIALTSSIMPL
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif


//! ctor
calc::FieldNewParameter::FieldNewParameter(
    const calc::ParsPar& par, bool constant, bool input, VS vs, ST st):
   calc::FieldParameter(par,constant,input, vs,st),
   d_value(nrElements(),0)
{
}

//! dtor
calc::FieldNewParameter::~FieldNewParameter()
{
  for(size_t i=0; i < d_value.size(); i++)
    delete d_value[i];
}

//! throw com::Exception if name validation fails
void calc::FieldNewParameter::moreValidation(
  const std::string& fileName) const
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
  for(size_t i=0; i < d_value.size(); i++) {
   if (fieldType().spatial()) {
    d_value[i] = new SpatialImpl(*this,i);
   } else {
    if (reportedInDynamic())
      d_value[i] = new NonSpatialTssImpl(*this,i);
    else
      d_value[i] = new NonSpatialImpl(*this,i);
    }
  }
}


//! return the parameter value
calc::Handle<calc::Field> calc::FieldNewParameter::value(
  size_t index,
  bool lastUse)
{
  PRECOND(index < nrElements());
  return d_value[index]->value(lastUse);
}

void calc::FieldNewParameter::assign(
    Handle<Field> f,
    size_t index,
    const Position *assignPoint)
{
  PRECOND(index < nrElements());
  d_value[index]->assign(f,assignPoint);
}
