#ifndef INCLUDED_CALC_TSSINPUTLEAF
#define INCLUDED_CALC_TSSINPUTLEAF

#ifndef INCLUDED_CALC_SYMBOL
# include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_CALC_VS
# include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_MEMORY
# include <memory>
#define INCLUDED_MEMORY
#endif

namespace calc {

class UsePar;
class TimeTable;
class InfoScript;
class IndexSelected;

//! manage timeinput tss parameter
class TssInputLeaf : public Symbol {
   class TssInputParameter *d_par;
   std::auto_ptr<IndexSelected> d_index;
public:
 TssInputLeaf(
   UsePar& par,
   VS resultVs);
 virtual ~TssInputLeaf();

  // ACCESSORS
  size_t select() const;
  void print(InfoScript& i)const;
  const TimeTable *execute();
};

}

#endif
