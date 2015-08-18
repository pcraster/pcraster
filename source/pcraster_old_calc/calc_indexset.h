#ifndef INCLUDED_CALC_INDEXSET
#define INCLUDED_CALC_INDEXSET

#ifndef INCLUDED_CALC_USERSYMBOL
# include "calc_usersymbol.h"
# define INCLUDED_CALC_USERSYMBOL
#endif

#ifndef INCLUDED_CALC_INDEXCONTAINER
# include "calc_indexcontainer.h"
# define INCLUDED_CALC_INDEXCONTAINER
#endif

namespace calc {

class IndexParameterConstant;
class IndexParameter;
class ArrayDefinition;
class InfoScript;

class IndexSet : public UserSymbol, public IndexContainer {
  bool d_on;
  Set d_set;
protected:
  void addToSet(Set& setToBeAddedTo)const;
public:
  IndexSet(const Symbol& name,
    Set set,
    bool d_on,
    ArrayDefinition *a);
  // ACCESSORS
  VS symbolType() const;
  void printSpecific(InfoScript& is)const;
  bool isOn()const;
};

}

#endif
