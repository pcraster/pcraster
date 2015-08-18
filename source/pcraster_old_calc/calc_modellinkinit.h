#ifndef INCLUDED_CALC_MODELLINKINIT
#define INCLUDED_CALC_MODELLINKINIT

#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
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
class Symbol;

/*!
    \brief model link initiation

   create when this statement is parsed:
   \code
      modellink modelId = modelname([strArg],args);
   \endcode
 */
class ModelLinkInit : public Statement {
    //! parameter with name modelId
    UserModelLink *d_par;

    //! for syntax check, postion of model in statement
    const Element            d_posOfModelName;
    //! all field args, so not the strArg
    const FieldExprArgs       d_args;
    ModelLinkMethodSignature d_sig;
public:
  ModelLinkInit(
    const Symbol& modelId,
    const Symbol& modelName,
    const std::string& strArg,
          FieldExprArgs& args);

  virtual ~ModelLinkInit();

  bool buildTypes();

  void prepareExecution();
  void run();
};

}

#endif
