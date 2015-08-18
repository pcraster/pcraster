#ifndef INCLUDED_CALC_INDEXPARAMETERVARIABLE
#define INCLUDED_CALC_INDEXPARAMETERVARIABLE

#ifndef INCLUDED_CALC_INDEXPARAMETER
#include "calc_indexparameter.h"
#define INCLUDED_CALC_INDEXPARAMETER
#endif

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
  size_t index() const;
  const IndexParameterConstant* indexParameterConstant() const;

  bool isOn()const { return true; };
};

}

#endif
