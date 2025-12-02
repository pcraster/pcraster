#include "stddefx.h"
#include "calc_findsymbol.h"
#include "app.h"
#include "misc.h"
#include "com_exception.h"
#include "calc_externalsymbols.h"
#include "calc_modellink.h"
#include "calc_operator.h"
#include "calc_quote.h"
#include <sstream>
#include <vector>
#include <map>

//------------------------------------------------------------------------------

namespace calc
{

class ParsArgTable
{
  std::vector<Operator> d_ops;
  typedef std::map<std::string, MAJOR_CODE> FuncMap;
  FuncMap d_funcs;

public:
  ParsArgTable()
  {
#include "operator.inc"
  }

  MAJOR_CODE findFunc(const std::string &name)
  {
    auto pos = d_funcs.find(name);
    if (pos != d_funcs.end())
      return pos->second;
    return OP_NOP;
  }

  const Operator &op(MAJOR_CODE code) const
  {
    PRECOND(((size_t)code) < d_ops.size());
    return d_ops[((size_t)code)];
  }

  size_t nrInternalOpCodes() const
  {
    return d_ops.size();
  }
};

static ParsArgTable parsArgTable;

static void loadExternalSymbols();
static size_t GetMinor(MAJOR_CODE op);

size_t nrInternalOpCodes()
{
  return parsArgTable.nrInternalOpCodes();
}

static MAJOR_CODE FindFunc(const char *name, bool loadExtLibs)
{
  MAJOR_CODE const r = parsArgTable.findFunc(name);
  if (r != OP_NOP)
    return r;
  if (!loadExtLibs)
    return OP_NOP;

  // otherwise not a standard function
  // may look for an external function
  if (ExternalSymbols::instance()->nrLibrariesLoaded() == 0) {
    loadExternalSymbols();
  }

  const calc::Operator *extOp = ExternalSymbols::instance()->find(name);
  if (extOp)
    return extOp->opCode();
  return OP_NOP;
}

static void loadExternalSymbols()
{

  size_t functionIndex = 0;   // First index of function loaded.
  size_t modelLinkIndex = 0;  // First index of model link loaded.
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
    for (size_t j = functionIndex; j < ExternalSymbols::instance()->nrExternalFunctions(); ++j) {
      functionName = ExternalSymbols::instance()->externFunc(j).name();

      if (FindFunc(functionName.c_str(), false) != OP_NOP || StrEq("if", functionName.c_str())) {
        // if is a special can
        std::ostringstream str;
        str << "dynamic library " << quote(libraryName) << ", function " << quote(functionName)
            << " already a known function name";
        throw com::Exception(str.str());
      }
    }

    for (size_t j = modelLinkIndex; j < ExternalSymbols::instance()->nrModelLinkProxies(); ++j) {
      modelLinkName = ExternalSymbols::instance()->modelLinkProxy(j).name();

      if (FindFunc(modelLinkName.c_str(), false) != OP_NOP || StrEq("if", modelLinkName.c_str())) {
        // if is a special can
        std::ostringstream str;
        str << "dynamic library " << quote(libraryName) << ", model link " << quote(modelLinkName)
            << " already a known function name";
        throw com::Exception(str.str());
      }
    }
  }
}

static size_t GetMinor(MAJOR_CODE op)
{
  PRECOND((size_t)op >= nrInternalOpCodes());
  size_t const minor = (size_t)(op)-nrInternalOpCodes();
  PRECOND(minor < ExternalSymbols::instance()->nrExternalFunctions());
  return minor;
}

}  // namespace calc

//------------------------------------------------------------------------------

const calc::Operator &calc::major2op(MAJOR_CODE major)
{
  auto maj = (size_t)major;
  if (maj < nrInternalOpCodes())
    return parsArgTable.op(major);
  else
    return ExternalSymbols::instance()->op(GetMinor(major));
}

//! lookup an operator by function name
/*!  returns the OP_NOP operator if opName if not a valid function name
 */
const calc::Operator &calc::funcName2op(const std::string &funcName)
{
  const MAJOR_CODE m = FindFunc(funcName.c_str(), true);

  if (m != OP_NOP)
    return major2op(m);
  return major2op(OP_NOP);
}

int calc::externalExecute(MAJOR_CODE major, void **out, const void **in, int nrArgs)
{
  size_t const minor = GetMinor(major);
  return (ExternalSymbols::instance()->externFunc(minor).Algorithm())(out, in, nrArgs);
}

void calc::externalBuildType(VS &newVs, bool &isSpatial, MAJOR_CODE major, const OP_ARGS *args,
                             int nrArgs)
{
  isSpatial = true;
  ST dummy;
  // CW check return value
  size_t const minor = GetMinor(major);
  (ExternalSymbols::instance()->externFunc(minor).ResultTypes())(&newVs, &dummy, nrArgs, args);
}

calc::ModelLink *calc::createModelLink(const std::string &name)
{
  // Check if model links have been loaded already.
  if (ExternalSymbols::instance()->nrLibrariesLoaded() == 0) {

    // Load model links from shared libraries.
    loadExternalSymbols();
  }

  calc::ModelLink *link = nullptr;

  // Get model link proxy for the model link requested.
  const calc::ModelLinkProxy *proxy = ExternalSymbols::instance()->findModelLinkProxy(name);

  if (proxy) {
    // Create the model link using the proxy.
    link = (*(proxy->creator()))();
  }

  return link;
}
