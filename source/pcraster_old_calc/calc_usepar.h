#ifndef  INCLUDED_CALC_USEPAR
#define  INCLUDED_CALC_USEPAR

#ifndef  INCLUDED_VECTOR
#include  <vector>
#define  INCLUDED_VECTOR
#endif

#ifndef  INCLUDED_CALC_PARSPAR
#include  "calc_parspar.h"
#define  INCLUDED_CALC_PARSPAR
#endif

#ifndef  INCLUDED_CALC_ARRAYDEFVECTOR
#include  "calc_arraydefvector.h"
#define  INCLUDED_CALC_ARRAYDEFVECTOR
#endif

namespace calc {

class IndexParameter;
class IndexSelected;
struct ConstructPar;

//! define par by it's use
class UsePar : public ParsPar {
  void init();
  std::vector<const IndexParameter *> d_selector;
  ArrayDefVector       d_descriptor;
public:
  UsePar(const ConstructPar& p);
  UsePar(StatementBlock *block, const Symbol& p);

  ~UsePar();

  const ArrayDefVector& descriptor() const;
  IndexSelected* createSelector() const;
};

}

#endif
