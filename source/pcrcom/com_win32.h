#ifndef  INCLUDED_COM_WIN32
#define  INCLUDED_COM_WIN32

#include <windows.h>
#include <string>


namespace com {
 const std::string win32GetLastError(void);
 const std::string win32ErrorString(DWORD errorCode);
 const std::string win32GetModuleFileName(void);
 const std::string win32ExceptionString(DWORD e);
};

#endif
