#ifndef INCLUDED_CALC_INDEXPARAMETER
#define INCLUDED_CALC_INDEXPARAMETER

#ifndef INCLUDED_CALC_PARAMETER
#include "calc_parameter.h"
#define INCLUDED_CALC_PARAMETER
#endif

#ifndef INCLUDED_CALC_INDEXCONTAINER
#include "calc_indexcontainer.h"
#define INCLUDED_CALC_INDEXCONTAINER
#endif

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
  void addToSet(std::set<const IndexParameter *>& setToBeAddedTo)const;
 public:
  // ACCESSORS
  VS symbolType() const;

  //! return to which IndexParameterConstant it is pointing
  virtual const IndexParameterConstant* indexParameterConstant() const=0;

  //! return active index of array
  virtual size_t index() const=0;
};


}

#endif
