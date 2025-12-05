#ifndef INCLUDED_OLDCALC_IDLIST
#define INCLUDED_OLDCALC_IDLIST

#include "calc_symbol.h"

#include <vector>


namespace calc {

//! holds an id-list
/*! An id-list can be empty
 */
class IdList {
private:
  std::vector<Symbol>d_idList;
public:
  //! create empty set
  IdList();
  //! create set from single id
  IdList(const Symbol& id);
  //! create from id-list
  IdList(const std::vector<Symbol>& idList);
  // ACCESSORS
  const Symbol& operator[](size_t nr) const;
  size_t size() const;
};
}

#endif
