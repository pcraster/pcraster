#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
#define INCLUDED_CALC_INDEXPARAMETERCONSTANT

#ifndef INCLUDED_CALC_INDEXPARAMETER
#include "calc_indexparameter.h"
#define INCLUDED_CALC_INDEXPARAMETER
#endif

namespace calc {

//! array index constant
/*!
 * array definition as found in the binding, each index is added as a constant
 * index parameter at global scope
 */
class  IndexParameterConstant : public IndexParameter {
 private:
   //! index in d_partOf
   size_t      d_indexInArray;
   //! is index used (+(default),-)
   const bool  d_on;
 public:
  // CREATORS
  IndexParameterConstant(const BindedSymbol& name,
                         bool on, const ArrayDefinition *def, size_t ind);


   bool isOn() const { return d_on; };

   //! the nominal nr it has in the array
   size_t index() const;

   const IndexParameterConstant* indexParameterConstant() const;
};


}

#endif
