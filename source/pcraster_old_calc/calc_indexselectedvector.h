#ifndef INCLUDED_OLDCALC_INDEXSELECTEDVECTOR
#define INCLUDED_OLDCALC_INDEXSELECTEDVECTOR

#include "calc_indexselected.h"
#include "calc_arraydefvector.h"

#include <vector>
#include <string>



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
  ~IndexSelectedVector() override;
  // ACCESSORS
  //! factor an ArrayDefVector
  ArrayDefVector arrayDefVector() const;
  int      select() const override;
  std::string arrayDefName() const;
  //! get element
  const class IndexParameter *operator[]( size_t n) const;
  //! get element
  size_t size() const { return d_vector.size();}
  std::string selectedName() const override;
  std::string variableName() const override;
  bool equal(const IndexSelectedVector *index2) const;
};

}

#endif
