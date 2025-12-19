#ifndef INCLUDED_CALC_TIMEINPUTSTACKOP
#define INCLUDED_CALC_TIMEINPUTSTACKOP

#include "stddefx.h"
#include "calc_iopimpl.h"



namespace calc {
  // TimeinputStackOp declarations.
}

namespace calc {


//! Operation Implementation
struct TimeinputStackOp: public IOpImpl {
  void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
};

struct LookupMapStack : public IOpImpl {
  void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
};


} // namespace calc

#endif
