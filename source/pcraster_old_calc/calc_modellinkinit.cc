#include "stddefx.h"
# include "calc_modellinkinit.h"
# include "calc_usermodellink.h"
# include "calc_modellink.h"
# include "calc_methodoperator.h"
# include "calc_fieldexpr.h"
# include "calc_fieldargs.h"
# include "calc_globargs.h"
# include "calc_fieldstack.h"
# include "calc_iscript.h"

/*!
 * \param strArg empty string if not given
 */
calc::ModelLinkInit::ModelLinkInit(
    const calc::Symbol& modelId,
    const calc::Symbol& modelName,
    const std::string& strArg,
          calc::FieldExprArgs& args):
    calc::Statement(modelId),
    d_posOfModelName(modelName),
    d_args(args),
    d_sig(strArg,d_args.size())
{
  try {
   d_par = new calc::UserModelLink(modelId, modelName);
   script().addSymbol(d_par);
   buildTypes();
  } catch(...) {
    cleanUp(args);
    throw;
  }
}

calc::ModelLinkInit::~ModelLinkInit()
{
  cleanUp(d_args);
}

bool calc::ModelLinkInit::buildTypes()
{
  for (size_t i=0; i < d_args.size(); i++) {
    d_args[i]->buildTypesRecursive(VS_FIELD);
    d_sig.d_input[i].vs = d_args[i]->vs();
    d_sig.d_input[i].st = d_args[i]->spatial() ? ST_SPATIAL : ST_NONSPATIAL;
  }

  d_par->initCheck(d_sig);

  calc::MethodOperator const mop(d_par->modelTypeName(), d_sig);

  calc::FieldArgs a(d_posOfModelName,mop(),d_args);
  a.restrictFieldArgs(d_sig.d_strArgGiven ? 1 : 0 );

  return false; // does not "create" fields
}

void calc::ModelLinkInit::prepareExecution()
{
  calc::MethodOperator const mop(d_par->modelTypeName(), d_sig);
  calc::FieldArgs a(d_posOfModelName,mop(),d_args);
  a.prepareExecution();
}

void calc::ModelLinkInit::run()
{
  calc::FieldStack stack;
  calc::MethodOperator const mop(d_par->modelTypeName(), d_sig);

  calc::FieldArgs a(d_posOfModelName,mop(),d_args);
  a.executeArgs(stack);

  calc::GlobArgs args(mop(), scriptConst().compressor(), stack);

  for (size_t i=0; i < d_args.size(); i++)
    d_sig.d_input[i].value = args.MAP_ptr(i);

  d_par->initExecute(d_sig);
}
