#include "stddefx.h"
#include "pcrcalc.h"
#include "pcrxsd_utils.h"
#include "com_file.h"
#include "calc_globallibdefs.h"  // globalLibDefs
#include "calc_catchallexceptions.h"
#include "calc_textscriptclientinterface.h"
#include "calc_xmlscriptclientinterface.h"

#include <iostream>
#include <sstream>

/*!
  \file
  This file contains the implementation of the Pcrcalc class.
*/


struct PcrcalcCreate {
  //! true if d_fileOrStr is a path name, false if d_fileOrStr is a string with script contents
  bool d_asFile;
  //! true if d_fileOrStr is a PCRasterXSD script element, false if it is a text script
  bool d_scriptXML;
  //! the script contents
  const char *d_fileOrStr;
  PcrcalcCreate(
   const bool asFile,
   const bool scriptXML,
   const char *fileOrStr):
      d_asFile(asFile),
      d_scriptXML(scriptXML),
      d_fileOrStr(fileOrStr)
  {}
};

/*!
 * Required gateway between C-Api (pcrcalc.h) and ClientInterface to
 * wrap each C-Api call for error (and exception) handling.
 *
 * \todo
 *  make sure only one instance is created. Is that needed?
 */
struct Pcrcalc
{
private:
  // the test drivers for the pcrcalc.h API
  friend class ClientInterfaceTest;

  //! empty if no error
  mutable std::ostringstream  d_errorStream;
  //! the buffer that remains valid
  std::string                 d_errorMsg;

  calc::ClientInterface      *d_ci;

  void clean() {
    if (d_ci)
      delete d_ci;
    d_ci=nullptr;
  }
  void cleanOnError() {
    if (!errorMessage().empty())
      clean();
  }

  //! Assignment operator. NOT IMPLEMENTED.
  Pcrcalc&           operator=           (Pcrcalc const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Pcrcalc               (Pcrcalc const& rhs);

  //! Default ctor. NOT IMPLEMENTED.
                   Pcrcalc               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    Pcrcalc              (PcrcalcCreate const& pc);

  /* virtual */    ~Pcrcalc              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              pcr_ScriptExecute    ();
  int               pcr_ScriptExecuteInitialStepMemory(DataTransferArray d);
  int               pcr_ScriptExecuteNextTimeStepMemory(DataTransferArray d);
  int               pcr_ScriptExecuteFinish();
  int               pcr_ScriptReleaseAllAllocatedMemory();
  const char*       pcr_ScriptXMLReflection        ();

  const std::string&  errorMessage()
  {
   d_errorMsg=d_errorStream.str();
   return d_errorMsg;
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  calc::ASTScript const& pcr_internalScript() const;

};


//------------------------------------------------------------------------------
// DEFINITION OF STATIC PCRCALC MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF PCRCALC MEMBERS
//------------------------------------------------------------------------------



Pcrcalc::Pcrcalc(PcrcalcCreate const& pc):
  d_ci(nullptr)
{
  calc::globalInit(); // pcr_createScriptFrom... call

  TRY_ALL {
    if (!pc.d_fileOrStr)
     throw com::Exception("call to pcr_createScriptFrom...() with 0 ptr argument");

    if (pc.d_scriptXML)
     d_ci = new calc::XMLScriptClientInterface(pc.d_fileOrStr,pc.d_asFile);
    else
     d_ci = new calc::TextScriptClientInterface(pc.d_fileOrStr,pc.d_asFile);
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
}



/* NOT IMPLEMENTED
//! Copy constructor.
Pcrcalc::Pcrcalc(Pcrcalc const& rhs)

  : Base(rhs)

{
}
*/


Pcrcalc::~Pcrcalc() {
  clean();
}


/* NOT IMPLEMENTED
//! Assignment operator.
Pcrcalc& calc::Pcrcalc::operator=(Pcrcalc const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

const char* Pcrcalc::pcr_ScriptXMLReflection()
{
  if (!d_ci)
   return nullptr;
 TRY_ALL {
    const char * result = d_ci->pcr_ScriptXMLReflection();
    return result;
    // char *resultB = (char *)CoTaskMemAlloc(strlen(result)+1);
    // strcpy( resultB, result);
    // return (char *) resultB;

  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
  return nullptr;
}

void Pcrcalc::pcr_ScriptExecute()
{
  TRY_ALL {
   if (d_ci) {
       d_ci->pcr_ScriptExecute();
   }
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
}

int Pcrcalc::pcr_ScriptExecuteInitialStepMemory(DataTransferArray d)
{
  if (!d_ci)
    return -1;
  TRY_ALL {
    return d_ci->pcr_ScriptExecuteInitialStepMemory(d);
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
  return -1;
}

int Pcrcalc::pcr_ScriptExecuteNextTimeStepMemory(DataTransferArray d)
{
  if (!d_ci)
    return -1;
  TRY_ALL {
    return d_ci->pcr_ScriptExecuteNextTimeStepMemory(d);
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
  return -1;
}

int Pcrcalc::pcr_ScriptReleaseAllAllocatedMemory()
{
  if (!d_ci)
    return -1;
  TRY_ALL {
    d_ci->pcr_ScriptReleaseAllAllocatedMemory();
    return 0;
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
  return -1;
}

int Pcrcalc::pcr_ScriptExecuteFinish()
{
  if (!d_ci)
    return -1;
  TRY_ALL {
    return d_ci->pcr_ScriptExecuteFinish();
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
  return -1;
}

calc::ASTScript const& Pcrcalc::pcr_internalScript() const
{
  PRECOND(d_ci);
  return d_ci->pcr_internalScript();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

static Pcrcalc* createScriptCommon(PcrcalcCreate const& pc)
{
  Pcrcalc *ps(nullptr);
  try {
   ps = new Pcrcalc(pc);
  } catch (...) {
    // only here on std::bad_alloc for the few
    // bytes of Pcrcalc
    // other errors are catched with the TRY_ALL
    delete ps;
    ps=nullptr;
  }
  return ps;
}

extern "C" PCR_ME_EXPORT Pcrcalc* pcr_createScriptFromXMLFile(
    const char* fileName)
{
  return createScriptCommon(PcrcalcCreate(true,true,fileName));
}

extern "C" PCR_ME_EXPORT PcrScript* pcr_createScriptFromTextFile(
    const char* scriptContents)
{
  return createScriptCommon(PcrcalcCreate(true,false,scriptContents));
}
extern "C" PCR_ME_EXPORT PcrScript* pcr_createScriptFromXMLString(const char* scriptContents)
{
  return createScriptCommon(PcrcalcCreate(false,true,scriptContents));
}

extern "C" PCR_ME_EXPORT PcrScript* pcr_createScriptFromTextString(const char* scriptContents)
{
  return createScriptCommon(PcrcalcCreate(false,false,scriptContents));
}

extern "C" PCR_ME_EXPORT const char * pcr_ScriptXMLReflection(
    PcrScript *script)
{
  if (!script)
     return nullptr; // BAD!
  return script->pcr_ScriptXMLReflection();
}

extern "C" PCR_ME_EXPORT void pcr_ScriptExecute(Pcrcalc *script)
{
  if (script)
    script->pcr_ScriptExecute();
}

extern "C" PCR_ME_EXPORT int pcr_ScriptExecuteInitialStepMemory(
    PcrScript *script,
    DataTransferArray dataTransferArray)
{
  if (!script)
    return -1; // BAD!
  return script->pcr_ScriptExecuteInitialStepMemory(dataTransferArray);
}

extern "C" PCR_ME_EXPORT int pcr_ScriptExecuteNextTimeStepMemory(
    PcrScript *script,
    DataTransferArray dataTransferArray)
{
  if (!script)
    return -1; // BAD!
  return script->pcr_ScriptExecuteNextTimeStepMemory(dataTransferArray);
}

/*
extern "C" PCR_DLL_FUNC(int) pcr_ScriptReleaseAllAllocatedMemory(
    PcrScript *script)
{
  if (!script)
    return -1; // BAD!
  return script->pcr_ScriptReleaseAllAllocatedMemory();
}
*/

extern "C" PCR_ME_EXPORT int pcr_ScriptExecuteFinish(
    PcrScript *script)
{
  if (!script)
    return -1; // BAD!
  return script->pcr_ScriptExecuteFinish();
}

extern "C" PCR_ME_EXPORT int pcr_ScriptError(Pcrcalc *script)
{
  if (!script)
    return -1; // BAD!
  return script->errorMessage().size();
}

extern "C" PCR_ME_EXPORT const  char* pcr_ScriptErrorMessage(Pcrcalc *script)
{
  if (!script)
    return "Error: called pcr_ScriptErrorMessage with 0 ptr";
 return script->errorMessage().c_str();
}

extern "C" PCR_ME_EXPORT void pcr_destroyScript(Pcrcalc *script)
{
  delete script;
}

calc::ASTScript const& pcr_internalScript(Pcrcalc *script)
{
  PRECOND(script);
  return script->pcr_internalScript();
}
