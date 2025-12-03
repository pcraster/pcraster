#include "stddefx.h"
#include "calc_nonspatialtssimpl.h"
#include "calc_fieldparameter.h"
#include "calc_nonspatial.h"

calc::NonSpatialTssImpl::NonSpatialTssImpl(const calc::FieldParameter &p, size_t index)
    : calc::NonSpatialImpl(p, index), d_tss(d_fw, 1, d_par.vs())
{
}

void calc::NonSpatialTssImpl::write()
{
  size_t dummy = 0;
  double *val = d_tss.getValueBuffer(dummy);
  if (!val) {  // do not write this time step
    return;
  }
  POSTCOND(dummy == 1);
  const auto *ns_val = dynamic_cast<const calc::NonSpatial *>(value());
  POSTCOND(ns_val);
  if (ns_val->isMv()) {
    SET_MV_REAL8(val);
  } else {
    *val = ns_val->getValue();
  }
}
