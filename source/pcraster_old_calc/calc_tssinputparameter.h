#ifndef INCLUDED_OLDCALC_TSSINPUTPARAMETER
#define INCLUDED_OLDCALC_TSSINPUTPARAMETER

#include "calc_tssparameter.h"

#include <vector>

namespace calc {

class TimeTable;

//! holds a tss
class  TssInputParameter : public TssParameter {
 public:
     typedef std::vector<TimeTable *>ValueVector;
 private:
     ValueVector d_vals;
 public:
  // CREATORS
  TssInputParameter(
    const ParsPar& par,
    bool constant,
       const std::vector<TimeTable *>& val);
  ~TssInputParameter() override;
  // ACCESSORS
  TimeTable *value(size_t index);
  VS vs() const override;
};


}

#endif 
