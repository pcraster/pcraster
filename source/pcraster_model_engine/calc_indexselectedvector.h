#ifndef INCLUDED_CALC_INDEXSELECTEDVECTOR
#define INCLUDED_CALC_INDEXSELECTEDVECTOR

#include "calc_indexselected.h"
#include "calc_arraydefvector.h"

#include <vector>
#include <string>


namespace calc {

//! an index vector on an arrayed parameter
/*!This holds selections, being astnumbers or variables
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
