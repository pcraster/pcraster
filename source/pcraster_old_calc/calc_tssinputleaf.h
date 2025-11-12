#ifndef INCLUDED_CALC_TSSINPUTLEAF
#define INCLUDED_CALC_TSSINPUTLEAF

# include "calc_symbol.h"
# include "calc_vs.h"

# include <memory>



namespace calc {

class UsePar;
class TimeTable;
class InfoScript;
class IndexSelected;

//! manage timeinput tss parameter
class TssInputLeaf : public Symbol {
   class TssInputParameter *d_par;
   std::unique_ptr<IndexSelected> d_index;
public:
 TssInputLeaf(
   UsePar& par,
   VS resultVs);
 ~TssInputLeaf() override;

  // ACCESSORS
  size_t select() const;
  void print(InfoScript& i)const;
  const TimeTable *execute();
};

}

#endif
