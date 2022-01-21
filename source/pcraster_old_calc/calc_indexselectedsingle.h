#ifndef INCLUDED_CALC_INDEXSELECTEDSINGLE
#define INCLUDED_CALC_INDEXSELECTEDSINGLE

#ifndef INCLUDED_CALC_INDEXSELECTED
#include "calc_indexselected.h"
#define INCLUDED_CALC_INDEXSELECTED
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


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
