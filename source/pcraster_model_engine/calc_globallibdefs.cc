#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif

// Library headers.
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

// PCRaster library headers.
#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_CLIBERROR
#include "com_cliberror.h"
#define INCLUDED_COM_CLIBERROR
#endif
#ifndef INCLUDED_COM_TUNE
#include "com_tune.h"
#define INCLUDED_COM_TUNE
#endif

// Module headers.
#ifndef INCLUDED_CALC_LIBRARYCLASS
#include "calc_LibraryClass.h"
#define INCLUDED_CALC_LIBRARYCLASS
#endif

/*!
  \file
  This file contains the implementation of a number of global definition
  needed to work with the old C-api stuff such as the misc,calc and api library
*/

//! throw a com::Exception as result of error handling nested errors
/*! the msg is a like Error was called, catch error nested and throw a StrErrorExcep
 * \todo what is the exact difference between libError() and throwLibError()?
 */
void calc::libError(const std::string& msg)
{
  com::ClibError::libError(msg);
}

//! Error (old style) already called
void calc::throwLibError(void)
{
  com::ClibError::throwLibError();
}

//! return lib error string
std::string calc::getLibError(void)
{
  return com::ClibError::getLibError();
}

struct ClientHolder : public calc::LibraryClassNoQt
{
  ClientHolder():
     calc::LibraryClassNoQt("PCRasterModelEngine")
     {}
};
static std::auto_ptr<ClientHolder> s_client(0);

//! all things we want to get rid off
/*!
 * Must be called at least once on dll/app/testdriver entrance.
 * Multiple calls should do no harm
 */
PCR_DLL_FUNC(void) calc::globalInit()
{
  // /home/cees/development/projects/DevEnv/sources/Utils/dev_XercesClient.h
  if (! s_client.get())
    s_client.reset(new ClientHolder());

  com::tune();

  // the mathx library
  SetRan(0); /* time seed */

  com::ClibError::reset();

  // the app library
  AppSetGlobalArgsDefaults();
}

/**
* needed for the Python extension
*/
PCR_DLL_FUNC(void) calc::setRan(size_t seed)
{
  SetRan(seed);
}

/**
* needed for the Python extension
*/
PCR_DLL_FUNC(int) calc::parseGlobalFlag(std::string const& option){
  return ParseGlobalFlag(("--" + option).c_str());
}

void calc::globalEnd()
{
  AppEnd();
  s_client.reset(0);
}
