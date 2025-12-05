#ifndef INCLUDED_OLDCALC_NONSPATIALTSSIMPL
#define INCLUDED_OLDCALC_NONSPATIALTSSIMPL

#include "calc_nonspatialimpl.h"
#include "calc_tssoutputvalue.h"



namespace calc {

class FieldParameter;

//! implements a newly create non spatial value that is written 
/*! This class is only used in FieldNewParameter::goInScope()
 */
class NonSpatialTssImpl : public NonSpatialImpl {
  TssOutputValue  d_tss;
public:
  NonSpatialTssImpl(const FieldParameter& p, size_t index);
  void write() override;
};

}

#endif
