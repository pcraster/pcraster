#ifndef INCLUDED_CALC_LOOKUPTABLELEAF
#define INCLUDED_CALC_LOOKUPTABLELEAF

#ifndef INCLUDED_CALC_SYMBOL
# include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_CALC_VS
# include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_VECTOR
# include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_MEMORY
# include <memory>
#define INCLUDED_MEMORY
#endif


namespace calc {

class UsePar;
class LookupTable;
class InfoScript;
class IndexSelected;

class LookupTableLeaf : public Symbol {
  class LookupTableParameter *d_par;
  std::auto_ptr<IndexSelected> d_index;
public:
  LookupTableLeaf(
    const UsePar& par,
    VS resultVs,
    const std::vector<VS>& readKeys,
    bool linear);
  virtual ~LookupTableLeaf();

  LookupTable *execute();

  // ACCESSORS
  size_t select() const;
  void print(InfoScript& i)const;
};

}

#endif
