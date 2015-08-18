#ifndef INCLUDED_CALC_NONSPATIALTSSIMPL
#define INCLUDED_CALC_NONSPATIALTSSIMPL

#ifndef INCLUDED_CALC_NONSPATIALIMPL
#include "calc_nonspatialimpl.h"
#define INCLUDED_CALC_NONSPATIALIMPL
#endif

#ifndef INCLUDED_CALC_TSSOUTPUTVALUE
#include "calc_tssoutputvalue.h"
#define INCLUDED_CALC_TSSOUTPUTVALUE
#endif

namespace calc {

class FieldParameter;

//! implements a newly create non spatial value that is written 
/*! This class is only used in FieldNewParameter::goInScope()
 */
class NonSpatialTssImpl : public NonSpatialImpl {
  TssOutputValue  d_tss;
public:
  NonSpatialTssImpl(const FieldParameter& p, size_t index);
  void write();
};

}

#endif
