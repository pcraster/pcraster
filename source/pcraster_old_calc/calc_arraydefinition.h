#ifndef INCLUDED_OLDCALC_ARRAYDEFINITION
#define INCLUDED_OLDCALC_ARRAYDEFINITION

#include "calc_usersymbol.h"
#include "calc_indexcontainer.h"

#include <vector>
#include <set>


namespace calc {

class ParsIndex;

//! definition of an array
/*! An array definition is of the form A = [ a1=ext1, a2=ext2, a3=ext3 ]
 *  <BR>
 *  The order of execution with a foreach is defined by the definition order
 *  (here a1,a2,a3)
 */
class ArrayDefinition : public UserSymbol, public IndexContainer {
   friend class ParsIndexName;
 private:
  // ordered vector of indices
   typedef std::vector<const class IndexParameterConstant *> IndexVector;

  //! list of active indices, in correct order 
  IndexVector d_activeIndex;
  //! list of indices that are off, prefixed by -
  IndexVector d_offIndex;
 protected:
  //! add all active indices to the set setToBeAddedTo
  void addToSet(std::set<const IndexParameter *>& setToBeAddedTo)const override;
 public:
  ArrayDefinition(
    const Symbol& name,
    const std::vector<ParsIndex *>& index);
  // ACCESSORS
  VS symbolType() const override;

  //! number of active elements
  size_t activeIndexSize()const;
  bool isOn()const override { return true; }

  const class IndexParameterConstant* item(size_t i) const;
  void printSpecific(InfoScript& i)const override;
};


}

#endif
