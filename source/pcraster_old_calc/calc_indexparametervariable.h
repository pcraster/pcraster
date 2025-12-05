#ifndef INCLUDED_OLDCALC_INDEXPARAMETERVARIABLE
#define INCLUDED_OLDCALC_INDEXPARAMETERVARIABLE

#include "calc_indexparameter.h"


namespace calc {

class ForEach;

//! array index constant
/*!
 * in foreach header, index is added as a variable at local scope
 */
class  IndexParameterVariable : public IndexParameter {
  const ForEach& d_foreach;
 public:
  // CREATORS
  IndexParameterVariable(const BindedSymbol& name,
                const ArrayDefinition *def,
                      ForEach* d_foreach);
  // ACCESSORS
  const ForEach& forEach() const;

  //! the nominal nr it has in the array
  size_t index() const override;
  const IndexParameterConstant* indexParameterConstant() const override;

  bool isOn()const override { return true; }
};

}

#endif
