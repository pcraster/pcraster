#ifndef INCLUDED_CALC_TSSINPUTPARAMETER
#define INCLUDED_CALC_TSSINPUTPARAMETER

#ifndef INCLUDED_CALC_TSSPARAMETER
#include "calc_tssparameter.h"
#define INCLUDED_CALC_TSSPARAMETER
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

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
  ~TssInputParameter();
  // ACCESSORS
  TimeTable *value(size_t index);
  VS vs() const;
};


}

#endif 
