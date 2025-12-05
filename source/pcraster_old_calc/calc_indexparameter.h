#ifndef INCLUDED_OLDCALC_INDEXPARAMETER
#define INCLUDED_OLDCALC_INDEXPARAMETER

#include "calc_parameter.h"
#include "calc_indexcontainer.h"


namespace calc {

class  IndexParameterConstant;

//! array index
/*! an IndexParameter is created at: <UL>
 * <LI>array definition, each index is added as a constant
 * index parameter at global scope
 * <LI>in foreach header, index is added as a variable at local scope
 * </UL>
 */
class  IndexParameter : public Parameter, public IndexContainer {
 protected:

  // CREATORS
  IndexParameter(const BindedSymbol& name,bool constant,
                 const class ArrayDefinition* def);
  void addToSet(std::set<const IndexParameter *>& setToBeAddedTo)const override;
 public:
  // ACCESSORS
  VS symbolType() const override;

  //! return to which IndexParameterConstant it is pointing
  virtual const IndexParameterConstant* indexParameterConstant() const=0;

  //! return active index of array
  virtual size_t index() const=0;
};


}

#endif
