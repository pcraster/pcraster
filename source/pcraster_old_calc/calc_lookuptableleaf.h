#ifndef INCLUDED_OLDCALC_LOOKUPTABLELEAF
#define INCLUDED_OLDCALC_LOOKUPTABLELEAF

#include "calc_symbol.h"
#include "calc_vs.h"

#include <vector>
#include <memory>


namespace calc {

class UsePar;
class LookupTable;
class InfoScript;
class IndexSelected;

class LookupTableLeaf : public Symbol {
  class LookupTableParameter *d_par;
  std::unique_ptr<IndexSelected> d_index;
public:
  LookupTableLeaf(
    const UsePar& par,
    VS resultVs,
    const std::vector<VS>& readKeys,
    bool linear);
  ~LookupTableLeaf() override;

  LookupTable *execute();

  // ACCESSORS
  size_t select() const;
  void print(InfoScript& i)const;
};

}

#endif
