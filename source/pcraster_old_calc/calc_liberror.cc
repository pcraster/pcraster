#include "stddefx.h"

#ifndef INCLUDED_CALC_LIBERROR
#include "calc_liberror.h"
#define INCLUDED_CALC_LIBERROR
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_MISC
#include "misc.h"   // Error
#define INCLUDED_MISC
#endif

static std::string libErrorStr;
static bool errorHandlerCalled=false;

//! setup handler for Error of libmisc in main
extern "C" void HandleLibError(const char *msg)
{
  libErrorStr = msg;
  errorHandlerCalled=true;
}

//! throw a com::Exception as result of error handling nested errors
void libError(const std::string& msg)
{
  // now let old style Error function call HandleLibError
  if(!errorHandlerCalled)
    Error(msg.c_str());
  else
    libErrorStr = msg + libErrorStr;

  throwLibError();
}

void throwLibError(void)
{
  errorHandlerCalled = false;

  // now throw what is stored
  throw com::Exception(libErrorStr);
}

//! return lib error string
std::string getLibError(void)
{
  errorHandlerCalled = false;
  return libErrorStr;
}
