#ifndef INCLUDED_CALC_MODELLINKMETHODEXPR
#define INCLUDED_CALC_MODELLINKMETHODEXPR

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDTYPE
# include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif
#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_FIELDEXPRARGS
#endif

#ifndef INCLUDED_MODELLINK
# include "calc_modellink.h"
#define INCLUDED_MODELLINK
#endif

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
  void skipExecution();
  const FieldType &fieldType()const;
public:
  ModelLinkMethodExpr(
    const Symbol& modelInstanceName,
    const Symbol& methodName,
    const std::string& strArg,
          FieldExprArgs& args);
   virtual ~ModelLinkMethodExpr();

  //! buids its own types and call for sub-expression
  void buildTypesRecursive(VS resultVsSet);

  FieldType& restrictType();

  void prepareExecution();
  void execute(FieldStack& stack);

  // ACCESSORS
  void print(InfoScript &si) const;
};

}

#endif
