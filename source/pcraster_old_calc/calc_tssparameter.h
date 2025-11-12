#ifndef INCLUDED_CALC_TSSPARAMETER
#define INCLUDED_CALC_TSSPARAMETER

#include "calc_subparameter.h"


namespace pcrxml {
  class Data;
}

namespace calc {

//! holds a tss
class  TssParameter : public SubParameter {
 public:
  // CREATORS
  TssParameter(const ParsPar& par, bool constant, bool input);
  ~TssParameter() override {}

  // ACCESSORS

  VS symbolType() const override;

  //! vs of contents (as in timeinput<i>vs</i>(..))
  virtual VS vs() const=0;

  void setDataSubType(pcrxml::Data *d) const override;
};

}

#endif
