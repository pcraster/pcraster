#ifndef INCLUDED_CALC_TSSOUTPUTPARAMETER
#define INCLUDED_CALC_TSSOUTPUTPARAMETER

#ifndef INCLUDED_CALC_TSSPARAMETER
#include "calc_tssparameter.h"
#define INCLUDED_CALC_TSSPARAMETER
#endif

struct MAP_INT4;

namespace calc {

class TssOutputValue;
class WriteInfo;

//! holds a tss
class  TssOutputParameter : public TssParameter {
   VS d_vs;
   std::vector<TssOutputValue *> d_value;
 public:
  // CREATORS
  TssOutputParameter(
  const ParsPar& par,
  const WriteInfo& w,
  bool constant);
  ~TssOutputParameter() override;

  void AddTotss(size_t index, const void **args, bool isClassTss);

  // MANIPULATORS
  void setVs(VS vsOfOutput);
  void goInScope() override;

  // ACCESSORS
  VS vs() const override;
};

}

#endif
