#include "stddefx.h"

#ifndef INCLUDED_CALC_MODELLINKMETHODEXPR
# include "calc_modellinkmethodexpr.h"
#define INCLUDED_CALC_MODELLINKMETHODEXPR
#endif

#ifndef INCLUDED_CALC_MODELLINK
# include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

#ifndef INCLUDED_CALC_USERMODELLINK
# include "calc_usermodellink.h"
#define INCLUDED_CALC_USERMODELLINK
#endif

#ifndef INCLUDED_CALC_PARSPAR
# include "calc_parspar.h"
#define INCLUDED_CALC_PARSPAR
#endif

#ifndef INCLUDED_CALC_ISCRIPT
# include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDARGS
# include "calc_fieldargs.h"
#define INCLUDED_CALC_FIELDARGS
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
# include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_FIELDLEFT
# include "calc_fieldleft.h"
#define INCLUDED_CALC_FIELDLEFT
#endif

#ifndef INCLUDED_CALC_FIELDTYPE
# include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

#ifndef INCLUDED_CALC_FIELDARGS
# include "calc_fieldargs.h"
#define INCLUDED_CALC_FIELDARGS
#endif

#ifndef INCLUDED_CALC_GLOBARGS
# include "calc_globargs.h"
#define INCLUDED_CALC_GLOBARGS
#endif
#ifndef INCLUDED_CALC_GLOBRESULT
#include "calc_globresult.h"
#define INCLUDED_CALC_GLOBRESULT
#endif

#ifndef INCLUDED_CALC_METHODOPERATOR
# include "calc_methodoperator.h"
#define INCLUDED_CALC_METHODOPERATOR
#endif

#ifndef INCLUDED_CALC_SPATIAL
# include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
# include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

calc::ModelLinkMethodExpr::ModelLinkMethodExpr(
    const calc::Symbol& modelName,
    const calc::Symbol& methodName,
    const std::string& strArg,
          calc::FieldExprArgs& args):
  calc::FieldExpr(modelName),
  d_fieldType(VS_FIELD, ST_DERIVED),
  d_par(dynamic_cast<calc::UserModelLink *>
         (script().findSymbol(&modelName,VS_OBJECT,true))),
  d_methodName(methodName),
  d_sig(strArg,args.size()),
  d_args(args)
{
  try {
    buildTypes();
 } catch(...) {
    cleanUp(args);
    throw;
 }
}

calc::ModelLinkMethodExpr::~ModelLinkMethodExpr()
{
  cleanUp(d_args);
}

//! buids its own types and call for sub-expression
void calc::ModelLinkMethodExpr::buildTypes()
{
  PRECOND(d_args.size() == d_sig.d_input.size());
  for (size_t i=0; i < d_args.size(); i++) {
    d_sig.d_input[i].vs = d_args[i]->vs();
    d_sig.d_input[i].st = d_args[i]->spatial() ?
                  ST_SPATIAL : ST_NONSPATIAL;
  }

  try {
    if ( ! d_par->methodCheck(d_methodName.name(),d_sig)) {
      // pcrcalc/test325
      d_methodName.posError(d_methodName.qName()+" unknown function for modellink "+
        d_par->modelTypeName());
    }
  } catch (calc::ModelLinkException& s) {
      // pcrcalc/test326
      d_methodName.posError(d_methodName.qName()+" "+s.what());
  }
  if (d_sig.d_result.size() != 1) // pcrcalc/test331
    posError("Expecting only one return value");

  d_fieldType.restrictUser(d_sig.d_result[0].vs, d_sig.d_result[0].st);

  calc::MethodOperator mop(d_methodName.name(), d_sig);

  calc::FieldArgs a(d_methodName,mop(),d_args);
  a.restrictFieldArgs(d_sig.d_strArgGiven ? 1 : 0 );
}

//! buids its own types and call for sub-expression
void calc::ModelLinkMethodExpr::buildTypesRecursive(VS resultVsSet)
{
  PRECOND(resultVsSet != VS_UNKNOWN);
  for(size_t i = 0; i < d_args.size(); i++)
    d_args[i]->buildTypesRecursive(vs());
  buildTypes();
}

calc::FieldType& calc::ModelLinkMethodExpr::restrictType()
{
  return d_fieldType;
}

const calc::FieldType &calc::ModelLinkMethodExpr::fieldType()const
{
  return d_fieldType;
}

void calc::ModelLinkMethodExpr::prepareExecution()
{
  MethodOperator mop(d_par->modelTypeName(), d_sig);
  FieldArgs a(d_methodName,mop(),d_args);
  a.prepareExecution();
}

void calc::ModelLinkMethodExpr::skipExecution()
{
  MethodOperator mop(d_par->modelTypeName(), d_sig);
  FieldArgs a(d_methodName,mop(),d_args);
  a.skipExecution();
}

void calc::ModelLinkMethodExpr::execute(FieldStack& stack)
{
  // pcrcalc/test333
  MethodOperator mop(d_par->modelTypeName(), d_sig);

  FieldArgs a(d_methodName,mop(),d_args);
  a.executeArgs(stack);

  GlobResult result(d_sig.d_result[0].vs, vs(), scriptConst().compressor());
  GlobArgs args(mop(), scriptConst().compressor(), stack);

  //! wrap inputs args into signature
  d_sig.d_result[0].value = result.MAPinterface();
  PRECOND(d_args.size() == d_sig.d_input.size());
  for (size_t i=0; i < d_args.size(); i++)
    d_sig.d_input[i].value = args.MAP_ptr(i);

  d_par->methodExecute(d_methodName.name(),d_sig);

  stack.push(result.createField());
}


void calc::ModelLinkMethodExpr::print(calc::InfoScript &si) const
{
 calc::InfoScript *s = &si;
 POSTCOND(!s);
}
