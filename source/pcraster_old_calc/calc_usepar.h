#ifndef  INCLUDED_CALC_USEPAR
#define  INCLUDED_CALC_USEPAR

#include  "calc_parspar.h"
#include  "calc_arraydefvector.h"

#include  <vector>


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

  ~UsePar() override;

  const ArrayDefVector& descriptor() const override;
  IndexSelected* createSelector() const;
};

}

#endif
