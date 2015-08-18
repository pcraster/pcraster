#ifndef INCLUDED_CALC_TSSPARAMETER
#define INCLUDED_CALC_TSSPARAMETER

#ifndef INCLUDED_CALC_SUBPARAMETER
#include "calc_subparameter.h"
#define INCLUDED_CALC_SUBPARAMETER
#endif

namespace pcrxml {
  class Data;
}

namespace calc {

//! holds a tss
class  TssParameter : public SubParameter {
 public:
  // CREATORS
  TssParameter(const ParsPar& par, bool constant, bool input);
  virtual ~TssParameter() {};

  // ACCESSORS

  VS symbolType() const;

  //! vs of contents (as in timeinput<i>vs</i>(..))
  virtual VS vs() const=0;

  void setDataSubType(pcrxml::Data *d) const;
};

}

#endif
