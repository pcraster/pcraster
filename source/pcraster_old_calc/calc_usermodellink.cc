#include "stddefx.h"
#include "calc_usermodellink.h"
#include "calc_iscript.h"
#include "calc_findsymbol.h"
#include "calc_modellink.h"

//! ctor
calc::UserModelLink::UserModelLink(const Symbol &parName, const Symbol &modelName) : UserSymbol(parName)
{
  d_modelInstance = createModelLink(modelName.name());
  if (!d_modelInstance) {
    // pcrcalc/test319
    modelName.posError(modelName.qName() + " No such modellink");
  }
}

#include <iostream>

/*!
 *  \bug the delete crashes. Is the factored  deleted,
 *  after the library is closed?; making code not accessible?
 *  I (CW) think test/pcrcalc/test333 and test/pcrcalc/test330 suffer from that 
 *  problem?????????
 *  on Win32
 */
calc::UserModelLink::~UserModelLink()
{
#ifndef WIN32
  delete d_modelInstance;
  d_modelInstance = nullptr;
#else
#ifdef DEBUG_DEVELOP
  std::cerr << "Warning: ModelLink instance dtor not called: memory leak\n";
#endif
#endif
}

void calc::UserModelLink::initCheck(calc::ModelLinkMethodSignature &s) const
{
  d_modelInstance->initCheck(s);
}

bool calc::UserModelLink::methodCheck(const std::string &name, calc::ModelLinkMethodSignature &s) const
{
  return d_modelInstance->methodCheck(name, s);
}

void calc::UserModelLink::methodExecute(const std::string &name, calc::ModelLinkMethodSignature &s)
{
  d_modelInstance->methodExecute(name, s);
}

void calc::UserModelLink::initExecute(calc::ModelLinkMethodSignature &s) const
{
  d_modelInstance->initExecute(scriptConst().rasterSpace(), s);
}

const std::string &calc::UserModelLink::modelTypeName() const
{
  return d_modelInstance->name();
}

VS calc::UserModelLink::symbolType() const
{
  return VS_OBJECT;
}
