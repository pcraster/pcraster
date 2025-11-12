#ifndef INCLUDED_CALC_FIELDNRPARAMETER
#define INCLUDED_CALC_FIELDNRPARAMETER

#include "calc_fieldparameter.h"
#include "calc_field.h"

#include <vector>


namespace calc {

class NonSpatialImpl;

//! constant, non-spatial parameters
/*! Created when:
 *  <UL>
 *   <LI> a = 3; encountered in binding
 *   <LI> a[A] = indexscalar(jan.tbl)
 *  </UL>
 */
class  FieldNrParameter : public FieldParameter {
 private:
     std::vector<NonSpatialImpl *> d_vals;
     std::vector<double> d_initVals;
 public:
    FieldNrParameter( const ParsPar& par, bool constant,
       const std::vector<double>& val, VS vs);
    FieldNrParameter(const ParsPar& par, double value, VS vs);

    ~FieldNrParameter() override;

  void goInScope() override;

  //! push parameter value on stack
  FieldHandle value(size_t index, bool lastUse) override;

  //  ACCESSORS

  double initValue(size_t index) const;

  double initialValue() const override;

};

}

#endif
