#include "stddefx.h"
#include "calc_globallibdefs.h"
#include "mathx.h"
#include "appargs.h"
#include "com_exception.h"
#include "com_cliberror.h"
#include "com_tune.h"
#include "calc_LibraryClass.h"

#include <memory>

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
void calc::throwLibError()
{
  com::ClibError::throwLibError();
}

//! return lib error string
std::string calc::getLibError()
{
  return com::ClibError::getLibError();
}

struct ClientHolder : public calc::LibraryClassNoQt
{
  ClientHolder():
     calc::LibraryClassNoQt("PCRasterModelEngine")
     {}
};
static std::unique_ptr<ClientHolder> s_client(nullptr);

//! all things we want to get rid off
/*!
 * Must be called at least once on dll/app/testdriver entrance.
 * Multiple calls should do no harm
 */
PCR_ME_EXPORT void calc::globalInit()
{
  // /home/cees/development/projects/DevEnv/sources/Utils/dev_XercesClient.h
  if (! s_client.get())
    s_client = std::make_unique<ClientHolder>();

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
PCR_ME_EXPORT void calc::setRan(size_t seed)
{
  SetRan(seed);
}

/**
* needed for the Python extension
*/
PCR_ME_EXPORT int calc::parseGlobalFlag(std::string const& option){
  return ParseGlobalFlag(("--" + option).c_str());
}

PCR_ME_EXPORT void calc::globalEnd()
{
  AppEnd();
  s_client.reset(nullptr);
}
