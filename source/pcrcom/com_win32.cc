#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef WIN32
#error this file should only be included for WIN32 platforms
#endif

#ifndef  INCLUDED_COM_WIN32
#include "com_win32.h"
#define  INCLUDED_COM_WIN32
#endif

//! path to exe or dll file this function is in, e.g. running module
const std::string com::win32GetModuleFileName(void)
{
  char dllFileNameCstr[2048] = "";
  GetModuleFileName(0,dllFileNameCstr,2048);
  return std::string(dllFileNameCstr);
}


//! return message from GetLastError(win32)
/*! GetLastError(win32) is like errno(3) and strerror(3) for win32
 */
const std::string com::win32GetLastError(void)
{
 return win32ErrorString(GetLastError());
}

//! return message string from errorCode
const std::string com::win32ErrorString(DWORD errorCode)
{
 char msgBuf[1024];
 std::string msg;

 FormatMessage(  FORMAT_MESSAGE_FROM_SYSTEM, NULL, errorCode,
                 MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                 (LPTSTR) &msgBuf, 1023, NULL);
 msg = msgBuf;
 return msg;
}

#ifndef INCLUDED_WIN32EXCEPTIONTEXT
# include "win32exceptiontext.inc"
#define INCLUDED_WIN32EXCEPTIONTEXT
#endif

//! return message string for exception code \a e
const std::string com::win32ExceptionString(DWORD e)
{
 return win32ExceptionText(e);
}
