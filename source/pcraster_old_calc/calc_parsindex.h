#ifndef INCLUDED_CALC_PARSINDEX
#define INCLUDED_CALC_PARSINDEX

# include "calc_symbol.h"
#include "calc_idlist.h"

#include <vector>



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
  ~ParsIndexName() override;
  UserSymbol *addMe(ArrayDefinition *a) const override;
};


class ParsIndexSet : public ParsIndex {
  Symbol  d_name;
  IdList  d_setList;
public:
  //! with list of symbols
  ParsIndexSet(bool on, const Symbol& name,
    const IdList&  setList);
  UserSymbol *addMe(ArrayDefinition *a) const override;
};

}

#endif
