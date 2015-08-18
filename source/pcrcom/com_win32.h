#ifndef  INCLUDED_COM_WIN32
#define  INCLUDED_COM_WIN32

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_WINDOWS
#include <windows.h>
#define INCLUDED_WINDOWS
#endif



namespace com {
 const std::string win32GetLastError(void);
 const std::string win32ErrorString(DWORD errorCode);
 const std::string win32GetModuleFileName(void);
 const std::string win32ExceptionString(DWORD e);
};

#endif
