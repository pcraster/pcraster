#ifndef INCLUDED_CALC_INDEXSELECTEDVECTOR
#define INCLUDED_CALC_INDEXSELECTEDVECTOR

#ifndef INCLUDED_INDEXSELECTED
#include "calc_indexselected.h"
#define INCLUDED_INDEXSELECTED
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif

namespace calc {

//! an index vector on an arrayed parameter
/*!This holds selections, being constants or variables
 *
 */
class  IndexSelectedVector :public  IndexSelected {
  std::vector<const class IndexParameter *>d_vector;
 public:
  // CREATORS
  IndexSelectedVector(
    std::vector<const class IndexParameter *>vector);
  virtual ~IndexSelectedVector();
  // ACCESSORS
  //! factor an ArrayDefVector
  ArrayDefVector arrayDefVector() const;
  int      select() const;
  std::string arrayDefName() const;
  //! get element
  const class IndexParameter *operator[]( size_t n) const;
  //! get element
  size_t size() const { return d_vector.size();};
  std::string selectedName() const;
  std::string variableName() const;
  bool equal(const IndexSelectedVector *index2) const;
};

}

#endif
