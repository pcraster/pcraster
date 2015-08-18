#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

// PCRaster library headers.
#ifndef INCLUDED_APP
#include "app.h"
#define INCLUDED_APP
#endif

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_EXTERNALSYMBOLS
#include "calc_externalsymbols.h"
#define INCLUDED_CALC_EXTERNALSYMBOLS
#endif

#ifndef INCLUDED_CALC_MODELLINK
#include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

//------------------------------------------------------------------------------

namespace calc {

class ParsArgTable {
  std::vector<Operator>                    d_ops;
  typedef std::map<std::string,MAJOR_CODE> FuncMap;
  FuncMap                                  d_funcs;
public:
  ParsArgTable() {
#include "operator.inc"
  }
  MAJOR_CODE findFunc(const std::string& name) {
    FuncMap::const_iterator pos= d_funcs.find(name);
    if (pos != d_funcs.end())
      return pos->second;
    return OP_NOP;
  }
  const Operator& op(MAJOR_CODE code) const {
    PRECOND( ((size_t)code) < d_ops.size());
    return d_ops[((size_t)code)];
  }
  size_t nrInternalOpCodes() const {
    return d_ops.size();
  }
};

static ParsArgTable parsArgTable;

static void loadExternalSymbols();
static size_t GetMinor(MAJOR_CODE op);

size_t nrInternalOpCodes() {
  return parsArgTable.nrInternalOpCodes();
}

static MAJOR_CODE FindFunc(const char* name, bool loadExtLibs)
{
  MAJOR_CODE r=parsArgTable.findFunc(name);
  if (r != OP_NOP)
    return r;
  if (!loadExtLibs)
    return OP_NOP;

  // otherwise not a standard function
  // may look for an external function
  if(ExternalSymbols::instance()->nrLibrariesLoaded() == 0) {
    loadExternalSymbols();
  }

  const calc::Operator *extOp= ExternalSymbols::instance()->find(name);
  if (extOp)
    return extOp->opCode();
  return OP_NOP;
}



static void loadExternalSymbols()
{

  size_t functionIndex;      // First index of function loaded.
  size_t modelLinkIndex;     // First index of model link loaded.
  std::string libraryName;
  std::string functionName;
  std::string modelLinkName;

  for (size_t i = 0; i < nrDynamicLibraryNames; ++i) {

    functionIndex = ExternalSymbols::instance()->nrExternalFunctions() - 1;
    modelLinkIndex = ExternalSymbols::instance()->nrModelLinkProxies() - 1;
    libraryName = dynamicLibraryNames[i];

    // Load symbols from library.
    ExternalSymbols::instance()->loadDynamicLibrary(libraryName);

    // Make sure symbol names read don't already exist.
    for(size_t j = functionIndex;
                   j < ExternalSymbols::instance()->nrExternalFunctions();
                   ++j) {
      functionName = ExternalSymbols::instance()->externFunc(j).name();

      if(FindFunc(functionName.c_str(), false) != OP_NOP ||
                   StrEq("if", functionName.c_str())) {
        // if is a special can
        std::ostringstream str;
        str << "dynamic library "
            << quote(libraryName)
            << ", function "
            << quote(functionName)
            << " already a known function name";
        throw com::Exception(str.str());
      }
    }

    for(size_t j = modelLinkIndex;
                   j < ExternalSymbols::instance()->nrModelLinkProxies(); ++j) {
      modelLinkName = ExternalSymbols::instance()->modelLinkProxy(j).name();

      if(FindFunc(modelLinkName.c_str(), false) != OP_NOP ||
                   StrEq("if", modelLinkName.c_str())) {
        // if is a special can
        std::ostringstream str;
        str << "dynamic library "
            << quote(libraryName)
            << ", model link "
            << quote(modelLinkName)
            << " already a known function name";
        throw com::Exception(str.str());
      }
    }
  }
}



static size_t GetMinor(MAJOR_CODE op)
{
  PRECOND((size_t)op >= nrInternalOpCodes());
  size_t minor = (size_t)(op)-nrInternalOpCodes();
  PRECOND(minor < ExternalSymbols::instance()->nrExternalFunctions());
  return minor;
}

}



//------------------------------------------------------------------------------

const calc::Operator& calc::major2op(MAJOR_CODE major)
{
  size_t maj = (size_t)major;
  if (maj < nrInternalOpCodes())
    return parsArgTable.op(major);
  else
    return ExternalSymbols::instance()->op(GetMinor(major));
}

//! lookup an operator by function name
/*!  returns the OP_NOP operator if opName if not a valid function name
 */
const calc::Operator& calc::funcName2op(const std::string& funcName)
{
  const MAJOR_CODE m = FindFunc(funcName.c_str(), true);

  if (m != OP_NOP)
    return major2op(m);
  return major2op(OP_NOP);
}

int calc::externalExecute(
  MAJOR_CODE major,
  void **out, const void **in, int nrArgs)
{
  size_t minor = GetMinor(major);
  return (ExternalSymbols::instance()->externFunc(minor).Algorithm())(out,in,nrArgs);
}

void calc::externalBuildType(
  VS& newVs, bool& isSpatial,
  MAJOR_CODE major,
  const OP_ARGS *args, int nrArgs)
{
  isSpatial = true;
  ST dummy;
  // CW check return value
  size_t minor = GetMinor(major);
  (ExternalSymbols::instance()->externFunc(minor).ResultTypes())(&newVs,&dummy,nrArgs,args);
}



calc::ModelLink* calc::createModelLink(const std::string& name)
{
  // Check if model links have been loaded already.
  if(ExternalSymbols::instance()->nrLibrariesLoaded() == 0) {

    // Load model links from shared libraries.
    loadExternalSymbols();
  }

  calc::ModelLink* link = 0;

  // Get model link proxy for the model link requested.
  const calc::ModelLinkProxy* proxy =
                   ExternalSymbols::instance()->findModelLinkProxy(name);

  if(proxy) {
    // Create the model link using the proxy.
    link = (*(proxy->creator()))();
  }

  return link;
}
