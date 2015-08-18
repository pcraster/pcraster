#ifndef INCLUDED_CALC_TIMEINPUTSTACKOP
#define INCLUDED_CALC_TIMEINPUTSTACKOP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_IOPIMPL
#include "calc_iopimpl.h"
#define INCLUDED_CALC_IOPIMPL
#endif



namespace calc {
  // TimeinputStackOp declarations.
}

namespace calc {


//! Operation Implementation
struct TimeinputStackOp: public IOpImpl {
  virtual void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};

struct LookupMapStack : public IOpImpl {
  virtual void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
};


} // namespace calc

#endif
