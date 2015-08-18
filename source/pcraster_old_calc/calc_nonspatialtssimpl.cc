#include "stddefx.h"

#ifndef INCLUDED_CALC_NONSPATIALTSSIMPL
#include "calc_nonspatialtssimpl.h"
#define INCLUDED_CALC_NONSPATIALTSSIMPL
#endif

#ifndef INCLUDED_CALC_FIELDPARAMETER
#include "calc_fieldparameter.h"
#define INCLUDED_CALC_FIELDPARAMETER
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

calc::NonSpatialTssImpl::NonSpatialTssImpl(
  const calc::FieldParameter& p, size_t index):
  calc::NonSpatialImpl(p,index),
  d_tss(d_fw,1,d_par.vs())
{
}

void calc::NonSpatialTssImpl::write()
{
  size_t dummy;
  double *val = d_tss.getValueBuffer(dummy);
  if (!val) // do not write this time step
    return;
  POSTCOND(dummy == 1);
  const calc::NonSpatial *ns_val = 
    dynamic_cast<const calc::NonSpatial *>(value());
  POSTCOND(ns_val);
  if (ns_val->isMv())
    SET_MV_REAL8(val);
  else
    *val = ns_val->getValue();
}
