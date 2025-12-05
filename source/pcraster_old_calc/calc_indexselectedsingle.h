#ifndef INCLUDED_OLDCALC_INDEXSELECTEDSINGLE
#define INCLUDED_OLDCALC_INDEXSELECTEDSINGLE

#include "calc_indexselected.h"

#include <string>


namespace calc {

class IndexSelectedSingle: public IndexSelected {
 public:
  ~IndexSelectedSingle() override;

  int      select()          const override;
  std::string selectedName() const override;
  std::string variableName() const override;
};

}

#endif
