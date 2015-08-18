#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CLIENTINTERFACE
#include "calc_clientinterface.h"
#define INCLUDED_CALC_CLIENTINTERFACE
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_XMLREFLECTION
#include "calc_xmlreflection.h"
#define INCLUDED_CALC_XMLREFLECTION
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_EXECUTOR
#include "calc_executor.h"
#define INCLUDED_CALC_EXECUTOR
#endif

/*!
  \file
  This file contains the implementation of the ClientInterface class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ClientInterfacePrivate
{
public:

  ClientInterfacePrivate()
  {
  }

  ~ClientInterfacePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

//! default ctor NOT IMPLEMENTED
calc::ClientInterface::ClientInterface()
{
}

//! ctor, no action such as parsing the scriptName is yet done
calc::ClientInterface::ClientInterface(
                   const std::string& scriptFileOrContents,
                   bool               asFile):
  d_script(0),
  d_executor(0),
  d_scriptFileOrContents(scriptFileOrContents),
  d_asFile(asFile)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::ClientInterface::ClientInterface(ClientInterface const& rhs)

  : Base(rhs)

{
}
*/



calc::ClientInterface::~ClientInterface()
{
  clean();
}

void calc::ClientInterface::clean()
{
  delete d_script;
  d_script = 0;
  delete d_executor;
  d_executor = 0;
}


/* NOT IMPLEMENTED
//! Assignment operator.
calc::ClientInterface& calc::ClientInterface::operator=(ClientInterface const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! load if not yet loaded
void calc::ClientInterface::load()
{
  if (d_script)
    return;
  d_script= createScriptAndAnalyzeNoContext();
}


//! return xml document for the script's reflection as a string
const char* calc::ClientInterface::pcr_ScriptXMLReflection()
{
  load();
  XMLReflection xmlReflection(*d_script);
  d_xmlReflectionBuffer=xmlReflection.toString();
  return d_xmlReflectionBuffer.c_str();
}

void calc::ClientInterface::pcr_ScriptExecute()
{
  load();
  d_script->resolve();

  if (d_script->symbols().containsMemoryExchangeSymbols())
    throw com::Exception("pcr_ScriptExecute can not execute a script with memoryExchange elements");

  Executor ex(
      d_script->cfgCode(),
      d_script->rteSettings(),
      d_script->symbols());
  ex.execAll();
}



//! doing the work for pcr_ScriptExecuteInitialStepMemory
int calc::ClientInterface::pcr_ScriptExecuteInitialStepMemory(
    void **data)
{
  PRECOND(data);// FTTB 0 may mean no fill/in and out
  load();

  // resolve possible lookuptables and so on
  d_script->resolve();

  if (d_executor)
    throw com::Exception("pcr_ScriptExecuteInitialStepMemory called twice");

  d_executor = new Executor(d_script->cfgCode(),
                            d_script->rteSettings(),
                            d_script->symbols());

  d_executor->runTimeEnv().setMemoryExchangeData(data);

  d_executor->startStepWise();
  if (d_executor->execInitialSection())
     return 0;
  return 1;
}

int calc::ClientInterface::pcr_ScriptExecuteNextTimeStepMemory(void **data)
{
  if (!d_executor)
    throw com::Exception("pcr_ScriptExecuteNextTimeStepMemory called with no prior call to pcr_ScriptExecuteInitialStepMemory");

  d_executor->runTimeEnv().setMemoryExchangeData(data);

  if (d_executor->execDynamicSectionOnce())
    return 0;
  return 1;
}

void calc::ClientInterface::pcr_ScriptReleaseAllAllocatedMemory(void)
{
  assert(false);
  if (d_executor) // TODO need this?
  { // nothing executed, nothing to do
    // for all allocated memory elements: release
  }
}

int calc::ClientInterface::pcr_ScriptExecuteFinish(void)
{
  d_executor->finishStepWise();
  clean();
  return 0;
}


//! only for internal verification of PCRasterModelEngine.dll
calc::ASTScript const& calc::ClientInterface::pcr_internalScript() const
{
  PRECOND(d_script);
  return *d_script;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



