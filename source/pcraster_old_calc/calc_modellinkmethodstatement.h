#ifndef INCLUDED_CALC_MODELLINKMETHODSTATEMENT
#define INCLUDED_CALC_MODELLINKMETHODSTATEMENT

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif

#ifndef INCLUDED_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_FIELDEXPRARGS
#endif
#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_MODELLINK
#include "calc_modellink.h"
#define INCLUDED_MODELLINK
#endif

namespace calc {

class UsePar;
class UserModelLink;
class WriteInfo;
class FieldLeft;
class StatementBlock;
struct ModelLinkArgSpec;



//! statement of form left0,left1,left2 = modelName::methodname([strArg],args)
/*! note that every invocation of a method is uniq, since for example the strArg
    may alter its operation completely
 */
class ModelLinkMethodStatement : public Statement {
    UserModelLink              *d_par;
  const Symbol                  d_methodName;
  const FieldExprArgs           d_args;
   ModelLinkMethodSignature     d_sig;
  std::vector<FieldLeft *>      d_left;

  void cleanUp();

public:
  ModelLinkMethodStatement(
    StatementBlock               *b,
    const WriteInfo&              w,
    const Symbol&                 modelInstanceName,
    const Symbol&                 methodName,
    const std::vector<UsePar>&    left,
    const std::string&            strArg,
    FieldExprArgs&                args);

  virtual ~ModelLinkMethodStatement();
  void prepareExecution();

  bool buildTypes();
  void run();
};


}

#endif
