#ifndef INCLUDED_CALC_FIELDNRPARAMETER
#define INCLUDED_CALC_FIELDNRPARAMETER

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_FIELDPARAMETER
#include "calc_fieldparameter.h"
#define INCLUDED_CALC_FIELDPARAMETER
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

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

    ~FieldNrParameter();

  void goInScope();

  //! push parameter value on stack
  FieldHandle value(size_t index, bool lastUse);

  //  ACCESSORS

  double initValue(size_t index) const;

  double initialValue() const;

};

}

#endif
