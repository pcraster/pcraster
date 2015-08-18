#ifndef INCLUDED_CALC_IDLIST
#define INCLUDED_CALC_IDLIST

#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

namespace calc {

typedef  std::vector<Id> IdList;

//! holds an id-list
/*! An id-list can be empty
 */
/*
class IdList {
private:
  std::vector<Id> d_idList;
public:
  //! create empty set
  IdList();
  //! create set from single id
  IdList(const Id& id);
  //! create from id-list
  IdList(const std::vector<Id>& idList);
  // ACCESSORS
  const Id& operator[](size_t nr) const;
  size_t size() const;
};
}
*/
}

#endif
