#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRCALC
#include "pcrcalc.h"
#define INCLUDED_PCRCALC
#endif

#ifndef INCLUDED_APPARGS
#include "appargs.h" // APP_IO_STRATEGY
#define INCLUDED_APPARGS
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif

#ifndef INCLUDED_CALC_CLIENTINTERFACE
#include "calc_clientinterface.h"
#define INCLUDED_CALC_CLIENTINTERFACE
#endif
#ifndef INCLUDED_CALC_WLDELFTHABITAT
#include "calc_wldelfthabitat.h"
#define INCLUDED_CALC_WLDELFTHABITAT
#endif

#ifndef INCLUDED_CALC_CATCHALLEXCEPTIONS
#include "calc_catchallexceptions.h"
#define INCLUDED_CALC_CATCHALLEXCEPTIONS
#endif


/*! \todo
 *  make sure only one instance is created
 * \todo
 *  when recognizing script or xml does this:
 *  <pre>
 *   if first non white space character is &lt;
 *    then XML (valid or not), otherwise a script and check for
 *    #! on first position
 *   Seems to do it for all our formats, except table:
 *   &lt; , 8> or &lt; 4 , ] ==> After &lt; first non-white space
 *   char not a "-","," or digit. Still holds XML validity.
 *  </pre>
 */
struct PcrScriptImpl {
public:
  //! empty if no error
  mutable std::ostringstream  d_errorStream;
  //! the buffer that remains valid
  std::string                 d_errorMsg;

  void clean() {
    delete d_ci; d_ci=0;
    delete d_wl; d_wl=0;
  }
  void cleanOnError() {
    if (!errorMessage().empty())
      clean();
  }
public:
  calc::ClientInterface  *d_ci;
  calc::WlDelftHabitat   *d_wl;

  PcrScriptImpl(const char* scriptName);

  ~PcrScriptImpl();

  void run();

  const std::string&  errorMessage()
  {
   d_errorMsg=d_errorStream.str();
   return d_errorMsg;
  }

};

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

extern std::string hackMsg;
static struct PcrScriptImpl *hackScript=0;

/*
 static void foo()
{
#ifdef DEBUG_DEVELOP
#ifdef BORLANDC
  // compiling with -xp on bcc32 will reveal this
  std::cerr << __throwExceptionName << "|"
            << __throwFileName      << "|"
            << __throwLineNumber    <<"\n";
#endif
#endif
  std::cerr << hackMsg;
  if (hackScript) {
    // werkt niet is te laat wrsl
    hackScript->d_errorStream << hackMsg;
  }
  exit(1);
}
*/

PcrScriptImpl::PcrScriptImpl(const char* scriptName):
      d_ci(0),d_wl(0)
{
    // multi-threaded, qt as shared lib needs this!
    // std::set_terminate(foo);

    bool unknownDE(false);
    TRY_ALL {
      if (!scriptName)
        throw com::Exception("call to pcr_createScript with 0 ptr argument");
      com::PathName pn(scriptName);
      try {
        pcrxml::Document doc(pn);
        QDomElement de(doc.documentElement());
        // if here then it is xml
        if(de.tagName() == "Root" &&
            (de.attribute("class") == "nl.wldelft.habitat.CaseHabitat" ||
             de.attribute("class") == "nl.wldelft.spatialanalysis.damage.CaseDamage")
          ){ appIOstrategy=APP_IO_BANDMAP;
             d_wl = new calc::WlDelftHabitat(pn);
        } else {
          unknownDE=true;
          std::ostringstream msg;
          msg << "document element '"
              << std::string(de.tagName().toLatin1())
              << "' is not a valid PCRaster script tag";
          throw com::FileFormatError(pn,msg.str());
        }
      } catch (...) {
        if (unknownDE)
          throw;
        // a normal script
        d_ci = new calc::ClientInterface();
        d_ci->setScriptFile(scriptName);
      }
    } CATCH_ALL_EXCEPTIONS(d_errorStream);
    cleanOnError();
    hackScript=this;
}

PcrScriptImpl::~PcrScriptImpl() {
  hackScript=0;
  clean();
}

void PcrScriptImpl::run()
{
  TRY_ALL {
       if (d_ci) {
           d_ci->run();
           if (d_ci->executeScriptStatus()==calc::ErrorExecScript)
              d_errorStream<<d_ci->errorMsg();
       }
       if (d_wl) {
           d_wl->parseXml();
           d_wl->execute();
       }
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
  cleanOnError();
}

/*
 */
extern "C" PCR_DLL_FUNC(PcrScriptImpl*) pcr_createScript(const char* scriptName)
{
  PcrScriptImpl *ps(0);
  try {
   ps = new PcrScriptImpl(scriptName);
  } catch (...) {
    // only here on std::bad_alloc for the few
    // bytes of PcrScriptImpl
    // other situs are catched with the TRY_ALL
    delete ps;
    ps=0;
  }
  return ps;
}

extern "C" PCR_DLL_FUNC(void) pcr_ScriptExecute(PcrScriptImpl *script)
{
  if (script)
    script->run();
}

/*! size of error message, 0 if none
 */
extern "C" PCR_DLL_FUNC(int) pcr_ScriptError(PcrScriptImpl *script)
{
  if (!script)
    return -1; // BAD!
  return script->errorMessage().size();
}
extern "C" PCR_DLL_FUNC(const  char*) pcr_ScriptErrorMessage(PcrScriptImpl *script)
{
  if (!script)
    return "Error: called pcr_ScriptErrorMessage with 0 ptr";
 return script->errorMessage().c_str();
}

extern "C" PCR_DLL_FUNC(void) pcr_destroyScript(PcrScriptImpl *script)
{
  delete script;
}
