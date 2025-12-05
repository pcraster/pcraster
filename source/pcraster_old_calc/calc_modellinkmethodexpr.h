#ifndef INCLUDED_OLDCALC_MODELLINKMETHODEXPR
#define INCLUDED_OLDCALC_MODELLINKMETHODEXPR

#include "calc_fieldexpr.h"
#include "calc_fieldtype.h"
#include "calc_symbol.h"
#include "calc_fieldexprargs.h"
#include "calc_modellink.h"



namespace calc {

class UserModelLink;

/*! a model link method that can be used as a field expression since
    it only returns 1 field.
    This is a special case of ModelLinkMethodStatement
 */
class ModelLinkMethodExpr : public FieldExpr{
private:
  FieldType                d_fieldType;
  UserModelLink           *d_par;
  const Symbol             d_methodName;
  ModelLinkMethodSignature d_sig;
  const FieldExprArgs      d_args;
  void buildTypes();
protected:
  void skipExecution() override;
  const FieldType &fieldType()const override;
public:
  ModelLinkMethodExpr(
    const Symbol& modelInstanceName,
    const Symbol& methodName,
    const std::string& strArg,
          FieldExprArgs& args);
   ~ModelLinkMethodExpr() override;

  //! buids its own types and call for sub-expression
  void buildTypesRecursive(VS resultVsSet) override;

  FieldType& restrictType() override;

  void prepareExecution() override;
  void execute(FieldStack& stack) override;

  // ACCESSORS
  void print(InfoScript &si) const override;
};

}

#endif
