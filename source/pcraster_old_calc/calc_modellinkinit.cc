#include "stddefx.h"

#ifndef INCLUDED_CALC_MODELLINKINIT
# include "calc_modellinkinit.h"
#define INCLUDED_CALC_MODELLINKINIT
#endif

#ifndef INCLUDED_CALC_USERMODELLINK
# include "calc_usermodellink.h"
#define INCLUDED_CALC_USERMODELLINK
#endif

#ifndef INCLUDED_CALC_MODELLINK
# include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

#ifndef INCLUDED_CALC_METHODOPERATOR
# include "calc_methodoperator.h"
#define INCLUDED_CALC_METHODOPERATOR
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDARGS
# include "calc_fieldargs.h"
#define INCLUDED_CALC_FIELDARGS
#endif

#ifndef INCLUDED_CALC_GLOBARGS
# include "calc_globargs.h"
#define INCLUDED_CALC_GLOBARGS
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
# include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_ISCRIPT
# include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

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

  calc::MethodOperator mop(d_par->modelTypeName(), d_sig);

  calc::FieldArgs a(d_posOfModelName,mop(),d_args);
  a.restrictFieldArgs(d_sig.d_strArgGiven ? 1 : 0 );

  return false; // does not "create" fields
}

void calc::ModelLinkInit::prepareExecution()
{
  calc::MethodOperator mop(d_par->modelTypeName(), d_sig);
  calc::FieldArgs a(d_posOfModelName,mop(),d_args);
  a.prepareExecution();
}

void calc::ModelLinkInit::run()
{
  calc::FieldStack stack;
  calc::MethodOperator mop(d_par->modelTypeName(), d_sig);

  calc::FieldArgs a(d_posOfModelName,mop(),d_args);
  a.executeArgs(stack);

  calc::GlobArgs args(mop(), scriptConst().compressor(), stack);

  for (size_t i=0; i < d_args.size(); i++)
    d_sig.d_input[i].value = args.MAP_ptr(i);

  d_par->initExecute(d_sig);
}
