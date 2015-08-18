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
  virtual ~IndexSelectedSingle();

  int      select()          const;
  std::string selectedName() const;
  std::string variableName() const;
};

}

#endif
