#include "stddefx.h"

#ifndef INCLUDED_CALC_MODELLINKMETHODSTATEMENT
# include "calc_modellinkmethodstatement.h"
#define INCLUDED_CALC_MODELLINKMETHODSTATEMENT
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_CALC_MODELLINK
# include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

#ifndef INCLUDED_CALC_USERMODELLINK
# include "calc_usermodellink.h"
#define INCLUDED_CALC_USERMODELLINK
#endif

#ifndef INCLUDED_CALC_USEPAR
# include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif

#ifndef INCLUDED_CALC_ISCRIPT
# include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
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

calc::ModelLinkMethodStatement::ModelLinkMethodStatement(
    calc::StatementBlock *b,
    const calc::WriteInfo& w,
    const calc::Symbol& modelName,
    const calc::Symbol& methodName,
    const std::vector<calc::UsePar>& left,
    const std::string& strArg,
          calc::FieldExprArgs& args):
    calc::Statement(left[0]),
      d_par(dynamic_cast<calc::UserModelLink *>
           (script().findSymbol(&modelName,VS_OBJECT,true))),
    d_methodName(methodName),
    d_args(args),
    d_sig(strArg,d_args.size())
{
 try {
  buildTypes();
  size_t nrArgExp = d_sig.d_result.size();
  if ( left.size() != nrArgExp ) {
    // pcrcalc/test327
    std::ostringstream msg;
    msg << "Expecting " << nrArgExp << " arguments left of =-symbol";
    posError(msg.str());
  }
  for (size_t i=0; i < left.size(); i++) {
    calc::ModelLinkArgSpec spec = d_sig.d_result[i];
    d_left.push_back(new calc::FieldLeft(b,w,left[i],spec.vs));
    calc::FieldType t(spec.vs,spec.st);
    d_left[i]->restrictUser(t); // pcrcalc/test328
  }
 } catch (...) {
    cleanUp();
    throw;
 }
}

calc::ModelLinkMethodStatement::~ModelLinkMethodStatement()
{
   cleanUp();
}

void calc::ModelLinkMethodStatement::cleanUp()
{
  calc::cleanUp(d_args);
  for (size_t i=0; i < d_left.size(); i++)
    delete d_left[i];
}

bool calc::ModelLinkMethodStatement::buildTypes()
{
  for (size_t i=0; i < d_args.size(); i++) {
    d_args[i]->buildTypesRecursive(VS_FIELD);
    d_sig.d_input[i].vs = d_args[i]->vs();
    d_sig.d_input[i].st = d_args[i]->spatial() ? ST_SPATIAL : ST_NONSPATIAL;
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

  MethodOperator mop(d_methodName.name(), d_sig);

  FieldArgs a(d_methodName,mop(),d_args);
  a.restrictFieldArgs(d_sig.d_strArgGiven ? 1 : 0 );

  return false; // ????????
}

void calc::ModelLinkMethodStatement::prepareExecution()
{
  MethodOperator mop(d_par->modelTypeName(), d_sig);
  FieldArgs a(d_methodName,mop(),d_args);
  a.prepareExecution();
  for (size_t i=0; i < d_left.size(); i++)
    d_left[i]->prepareExecution();
}

void calc::ModelLinkMethodStatement::run()
{
  FieldStack stack;
  MethodOperator mop(d_par->modelTypeName(), d_sig);

  FieldArgs a(d_methodName,mop(),d_args);
  a.executeArgs(stack);

  GlobArgs args(mop(), scriptConst().compressor(), stack);

  //! wrap inputs args into signature
  PRECOND(d_args.size() == d_sig.d_input.size());
  for (size_t i=0; i < d_args.size(); i++)
    d_sig.d_input[i].value = args.MAP_ptr(i);

  //! create the results
  typedef boost::shared_ptr<GlobResult> SGlobResult;
  std::vector<SGlobResult> result;
  for (size_t i=0; i < d_sig.d_result.size(); i++) {
    VS vs=d_sig.d_result[i].vs;
    result.push_back(SGlobResult(
        new GlobResult(vs,vs,scriptConst().compressor())));
    d_sig.d_result[i].value = result[i]->MAPinterface();
  }

  d_par->methodExecute(d_methodName.name(),d_sig);

  // CW for the time being like, this
  // but the nonspatial->spatial cast of Assignment must
  // be considered, if we wish to return nonspatials
  for (size_t i=0; i < d_sig.d_result.size(); i++)
    d_left[i]->assign(result[i]->createField());
}
