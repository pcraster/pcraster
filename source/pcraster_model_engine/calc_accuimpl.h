#ifndef INCLUDED_CALC_ACCUIMPL
#define INCLUDED_CALC_ACCUIMPL

#include "stddefx.h"
#include "calc_iopimpl.h"



namespace calc {
  class RunTimeEnv;
  class Operator;
}



namespace calc {

class AccuAll: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const override;
};

extern  AccuAll         accuAll;
#define Do_accu        &accuAll

class AccuCapacity: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const override;
};
extern AccuCapacity accuCapacity;
#define Do_accucapacity &accuCapacity

class AccuThreshold: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const override;
};
extern AccuThreshold accuThreshold;
#define Do_accuthreshold &accuThreshold

class AccuFraction: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const override;
};
extern AccuFraction accuFraction;
#define Do_accufraction &accuFraction

class AccuTrigger: public IOpImpl {
public:
  void             exec                 (RunTimeEnv* rte,
                                         const Operator& op,
                                         size_t nrArgs) const override;
};
extern AccuTrigger accuTrigger;
#define Do_accutrigger &accuTrigger


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

#endif
