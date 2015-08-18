#ifndef INCLUDED_CALC_USERMODELLINK
#define INCLUDED_CALC_USERMODELLINK

#ifndef INCLUDED_CALC_USERSYMBOL
#include "calc_usersymbol.h"
#define INCLUDED_CALC_USERSYMBOL
#endif

namespace calc {

class ModelLink;
class ModelLinkMethodSignature;

class UserModelLink : public UserSymbol {
  //! instance of link
  ModelLink* d_modelInstance;
public:
  //! the symbol type
  VS symbolType() const;
  UserModelLink(const Symbol& parName, const Symbol& modelName);

  virtual ~UserModelLink();

  void initCheck(ModelLinkMethodSignature& s) const;
  void initExecute(ModelLinkMethodSignature& s) const;
  bool methodCheck(const std::string& name, ModelLinkMethodSignature& s) const;
  void methodExecute(const std::string& name, ModelLinkMethodSignature& s);
  const std::string& modelTypeName() const;
};

}

#endif
