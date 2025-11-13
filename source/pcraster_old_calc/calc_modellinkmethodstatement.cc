#include "stddefx.h"
#include "calc_modellinkmethodstatement.h"
#include "calc_modellink.h"
#include "calc_usermodellink.h"
#include "calc_usepar.h"
#include "calc_iscript.h"
#include "calc_fieldexpr.h"
#include "calc_fieldstack.h"
#include "calc_fieldleft.h"
#include "calc_fieldtype.h"
#include "calc_fieldargs.h"
#include "calc_globargs.h"
#include "calc_globresult.h"
#include "calc_methodoperator.h"

#include <memory>

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
  size_t const nrArgExp = d_sig.d_result.size();
  if ( left.size() != nrArgExp ) {
    // pcrcalc/test327
    std::ostringstream msg;
    msg << "Expecting " << nrArgExp << " arguments left of =-symbol";
    posError(msg.str());
  }
  for (size_t i=0; i < left.size(); i++) {
    calc::ModelLinkArgSpec const spec = d_sig.d_result[i];
    d_left.push_back(new calc::FieldLeft(b,w,left[i],spec.vs));
    calc::FieldType const t(spec.vs,spec.st);
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
  for (auto & i : d_left)
    delete i;
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

  MethodOperator const mop(d_methodName.name(), d_sig);

  FieldArgs a(d_methodName,mop(),d_args);
  a.restrictFieldArgs(d_sig.d_strArgGiven ? 1 : 0 );

  return false; // ????????
}

void calc::ModelLinkMethodStatement::prepareExecution()
{
  MethodOperator const mop(d_par->modelTypeName(), d_sig);
  FieldArgs a(d_methodName,mop(),d_args);
  a.prepareExecution();
  for (auto & i : d_left)
    i->prepareExecution();
}

void calc::ModelLinkMethodStatement::run()
{
  FieldStack stack;
  MethodOperator const mop(d_par->modelTypeName(), d_sig);

  FieldArgs a(d_methodName,mop(),d_args);
  a.executeArgs(stack);

  GlobArgs args(mop(), scriptConst().compressor(), stack);

  //! wrap inputs args into signature
  PRECOND(d_args.size() == d_sig.d_input.size());
  for (size_t i=0; i < d_args.size(); i++)
    d_sig.d_input[i].value = args.MAP_ptr(i);

  //! create the results
  typedef std::shared_ptr<GlobResult> SGlobResult;
  std::vector<SGlobResult> result;
  for (size_t i=0; i < d_sig.d_result.size(); i++) {
    VS const vs=d_sig.d_result[i].vs;
    result.push_back(std::make_shared<GlobResult>(
        vs,vs,scriptConst().compressor()));
    d_sig.d_result[i].value = result[i]->MAPinterface();
  }

  d_par->methodExecute(d_methodName.name(),d_sig);

  // CW for the time being like, this
  // but the nonspatial->spatial cast of Assignment must
  // be considered, if we wish to return nonspatials
  for (size_t i=0; i < d_sig.d_result.size(); i++)
    d_left[i]->assign(result[i]->createField());
}
