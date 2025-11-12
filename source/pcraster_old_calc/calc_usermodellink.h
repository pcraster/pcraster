#ifndef INCLUDED_CALC_USERMODELLINK
#define INCLUDED_CALC_USERMODELLINK

#include "calc_usersymbol.h"


namespace calc {

class ModelLink;
class ModelLinkMethodSignature;

class UserModelLink : public UserSymbol {
  //! instance of link
  ModelLink* d_modelInstance;
public:
  //! the symbol type
  VS symbolType() const override;
  UserModelLink(const Symbol& parName, const Symbol& modelName);

  ~UserModelLink() override;

  void initCheck(ModelLinkMethodSignature& s) const;
  void initExecute(ModelLinkMethodSignature& s) const;
  bool methodCheck(const std::string& name, ModelLinkMethodSignature& s) const;
  void methodExecute(const std::string& name, ModelLinkMethodSignature& s);
  const std::string& modelTypeName() const;
};

}

#endif
