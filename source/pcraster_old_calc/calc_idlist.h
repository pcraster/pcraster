#ifndef INCLUDED_CALC_IDLIST
#define INCLUDED_CALC_IDLIST

#ifndef INCLUDED_CALC_SYMBOL
# include "calc_symbol.h"
# define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

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
