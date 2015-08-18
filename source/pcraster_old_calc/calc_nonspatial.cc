#include "stddefx.h"

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_CSF
#include "csf.h"       // IsMVcellRepr
#define INCLUDED_CSF
#endif

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

//! constructor
/*! default to zero, if we do not init later,
    is now needed for the skipped exection of an if branch,
    because the impls. will test on not-MV of a nonspatial
 */
calc::NonSpatial::NonSpatial(VS vs, double value):
  calc::Field(vs  )
{
  d_crVal = biggestCellRepr(vs);
  switch(d_crVal) {
   case CR_REAL4: d_vals = (REAL4)value; break;
   case CR_INT4:  d_val4 = (INT4)value; break;
   case CR_UINT1: d_val1 = (UINT1)value; break;
   default : POSTCOND(FALSE); // NEVER
  }
  POSTCOND(!isMv());
}

void calc::NonSpatial::analyzeBoolean(bool& noneAreTrue,bool& noneAreFalse) const
{
  PRECOND(vs() == VS_B);
  PRECOND(d_crVal == CR_UINT1);
  noneAreTrue = noneAreFalse = true;
  if (isMv())
    return;
  if ( ((int)getValue()) == 1 )
    noneAreTrue  = false;
  else
    noneAreFalse = false;
}

calc::NonSpatial::~NonSpatial()
{
}

size_t calc::NonSpatial::nrValues() const
{
  return 1;
}

void calc::NonSpatial::setCell(const double& value, size_t /* i */)
{
  if (IsMVcellRepr(CR_REAL8, &value)) {
    throw SetNonSpatialToMV();
  }
  switch(d_crVal) {
   case CR_REAL4: d_vals = (REAL4)value; break;
   case CR_INT4:  d_val4 = (INT4)value; break;
   case CR_UINT1: d_val1 = (UINT1)value; break;
   default : POSTCOND(FALSE); // NEVER
  }
}

bool calc::NonSpatial::getCell(double& value, size_t /* i */) const
{
  value = getValue();
  return true;
}

calc::NonSpatial *calc::NonSpatial::copy() const
{
  calc::NonSpatial *n = new calc::NonSpatial(vs() /* ,true */);
  n->d_crVal = d_crVal;
  switch(d_crVal) {
    case CR_REAL4: n->d_vals = d_vals; break;
    case CR_INT4:  n->d_val4 = d_val4; break;
    case CR_UINT1: n->d_val1 = d_val1; break;
    default : POSTCOND(FALSE); // NEVER
  }
  return n;
}

const void *calc::NonSpatial::srcValue() const
{
  PRECOND(!isMv()); // should have a value
  switch(d_crVal) {
    case CR_REAL4: return &d_vals;
    case CR_INT4:  return &d_val4;
    case CR_UINT1: return &d_val1;
    default : POSTCOND(FALSE); // NEVER
              return &d_vals; 
  }
}
void *calc::NonSpatial::destValue()
{
  switch(d_crVal) {
    case CR_REAL4: return &d_vals;
    case CR_INT4:  return &d_val4;
    case CR_UINT1: return &d_val1;
    default : POSTCOND(FALSE); // NEVER
              return &d_vals; 
  }
}

const void *calc::NonSpatial::voidValue() const
{
  switch(d_crVal) {
    case CR_REAL4: return &d_vals;
    case CR_INT4:  return &d_val4;
    case CR_UINT1: return &d_val1;
    default : POSTCOND(FALSE); // NEVER
              return &d_vals; 
  }
}

double calc::NonSpatial::getValue()const
{
  PRECOND(!isMv());
  switch(d_crVal) {
    case CR_REAL4: return (double)d_vals;
    case CR_INT4:  return (double)d_val4;
    case CR_UINT1: return (double)d_val1;
    default : POSTCOND(FALSE); // NEVER
               return (double)d_vals;
  }
}

bool calc::NonSpatial::isMv()const
{
  return IsMVcellRepr(d_crVal, voidValue()) != 0;
}

bool calc::NonSpatial::isSpatial() const
{
  return false;
}
