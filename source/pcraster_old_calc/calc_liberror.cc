#include "stddefx.h"
#include "calc_liberror.h"
#include "com_exception.h"
#include "misc.h"   // Error

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

void throwLibError()
{
  errorHandlerCalled = false;

  // now throw what is stored
  throw com::Exception(libErrorStr);
}

//! return lib error string
std::string getLibError()
{
  errorHandlerCalled = false;
  return libErrorStr;
}
