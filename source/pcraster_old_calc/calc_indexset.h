#ifndef INCLUDED_OLDCALC_INDEXSET
#define INCLUDED_OLDCALC_INDEXSET

#include "calc_usersymbol.h"
#include "calc_indexcontainer.h"



namespace calc {

class IndexParameterConstant;
class IndexParameter;
class ArrayDefinition;
class InfoScript;

class IndexSet : public UserSymbol, public IndexContainer {
  bool d_on;
  Set d_set;
protected:
  void addToSet(Set& setToBeAddedTo)const override;
public:
  IndexSet(const Symbol& name,
    Set set,
    bool d_on,
    ArrayDefinition *a);
  // ACCESSORS
  VS symbolType() const override;
  void printSpecific(InfoScript& is)const override;
  bool isOn()const override;
};

}

#endif
