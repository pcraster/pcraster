#ifndef INCLUDED_OLDCALC_MODELLINKINIT
#define INCLUDED_OLDCALC_MODELLINKINIT

#include "calc_statement.h"
#include "calc_fieldexprargs.h"
#include "calc_modellink.h"



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

  ~ModelLinkInit() override;

  bool buildTypes() override;

  void prepareExecution() override;
  void run() override;
};

}

#endif
