#ifndef INCLUDED_CALC_FIELDNEWPARAMETER
#define INCLUDED_CALC_FIELDNEWPARAMETER

#include "calc_fieldparameter.h"
#include "calc_field.h"

#include <vector>


namespace calc {

class FieldValue;
class Position;

//! parameter that is created by script.
/*! Since it is created by the script is does not have an initial value.
 *  At creation point, we do not know yet if it is going to be spatial or not.
 */
class  FieldNewParameter : public FieldParameter {
 protected:
  //! initialized to 0, goInScope() initializes all to non-0's
  std::vector<FieldValue *>d_value;

  void moreValidation(const std::string& fileName) const override;
 public:
   FieldNewParameter(const ParsPar& par, bool constant, bool input, VS vs, ST st);
  ~FieldNewParameter() override;

  // MODIFIERS

  void goInScope() override;
  //! assign current stack top to parameter
  void assign(const FieldHandle& f,size_t index, const Position *assignPoint);
  FieldHandle value(size_t index, bool lastUse) override;

};

}

#endif
