#ifndef INCLUDED_CALC_PARSINDEX
#define INCLUDED_CALC_PARSINDEX

#ifndef INCLUDED_CALC_SYMBOL
# include "calc_symbol.h"
# define INCLUDED_YMBOL
#endif

#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

namespace calc {

class  ArrayDefinition;
class  UserSymbol;

class  ParsIndex {
protected:
  bool d_on;
public:
  ParsIndex(bool on);
  bool On() const;
  virtual UserSymbol *addMe(ArrayDefinition *a) const=0;
  virtual ~ParsIndex();
};

class ParsIndexName : public ParsIndex {
  Symbol d_name;
  /*! 0, if no external name  defined */
  Symbol *d_extName;
public:
  ParsIndexName(bool on, const Symbol& name);
  ParsIndexName(bool on, const Symbol& name, const Symbol& extName);
  ~ParsIndexName();
  UserSymbol *addMe(ArrayDefinition *a) const;
};


class ParsIndexSet : public ParsIndex {
  Symbol  d_name;
  IdList  d_setList;
public:
  //! with list of symbols
  ParsIndexSet(bool on, const Symbol& name,
    const IdList&  setList);
  UserSymbol *addMe(ArrayDefinition *a) const;
};

}

#endif
