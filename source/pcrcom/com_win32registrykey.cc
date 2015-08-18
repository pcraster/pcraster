#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef WIN32
#error this file should only be included for WIN32 platforms
#endif


#ifndef INCLUDED_COM_WIN32REGISTRYKEY
#include "com_win32registrykey.h"
#define INCLUDED_COM_WIN32REGISTRYKEY
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_WIN32
#include "com_win32.h"
#define INCLUDED_COM_WIN32
#endif



/*!
  \file
  This file contains the implementation of the Win32RegistryKey class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC WIN32REGISTRYKEY MEMBERS
//------------------------------------------------------------------------------


namespace com {
  static HKEY topLevelKey(Win32RegistryKey::TopLevel tl)
  {
    switch(tl) {
      case Win32RegistryKey::CurrentUser:  return HKEY_CURRENT_USER;
      case Win32RegistryKey::LocalMachine: return HKEY_LOCAL_MACHINE;
    }
    return HKEY_LOCAL_MACHINE;
  }
};



//------------------------------------------------------------------------------
// DEFINITION OF WIN32REGISTRYKEY MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::Win32RegistryKey::Win32RegistryKey(
    TopLevel tl,
    const std::string&  subKey):
     d_tl(tl),d_subKey(subKey)
{
}

//! dtor
com::Win32RegistryKey::~Win32RegistryKey()
{
}

LONG com::Win32RegistryKey::open(HKEY& key) const
{
  return RegOpenKeyEx(
                 topLevelKey(d_tl),
                 d_subKey.c_str(),
                 0, // ulOptions
                 KEY_ALL_ACCESS, // or KEY_READ?
                 &key);
}


bool com::Win32RegistryKey::exists() const
{
  HKEY key;
  if (open(key) == ERROR_SUCCESS) {
    RegCloseKey(key);
    return true;
  }
  return false;
}

//! empty string if not existant
/*!
  *  \todo
  *    handle ERROR_MORE_DATA return of API call
  *  \todo
  *    check return of type
  */
std::string com::Win32RegistryKey::value() const
{
  if (exists()) {
    // std::string val=d_subKey+"\\(Default)";
    HKEY key;
    BYTE buf[MAX_PATH];
    DWORD type;
    DWORD len =MAX_PATH;
    LONG result;
    if (open(key) == ERROR_SUCCESS) {
     result = RegQueryValueEx(
                   key, // , topLevelKey(d_tl),
                   0, // d_subKKey.c_str(),
                   0,&type, buf,&len);
     RegCloseKey(key);
    }
    if (result == ERROR_SUCCESS)
        return std::string((const char *)buf);
  }
  return "";
}

//! empty string if not existant
/*!
  *  \todo
  *    handle ERROR_MORE_DATA return of API call
  *  \todo
  *    check return of type
  */
std::string com::Win32RegistryKey::value(const std::string& name) const
{
  if (exists()) {
    // std::string val=d_subKey+"\\(Default)";
    HKEY key;
    BYTE buf[MAX_PATH];
    DWORD type;
    DWORD len =MAX_PATH;
    LONG result;
    if (open(key) == ERROR_SUCCESS) {
     result = RegQueryValueEx(
                   key,
                   name.c_str(),
                   0,&type, buf,&len);
     RegCloseKey(key);
    }
    if (result == ERROR_SUCCESS)
        return std::string((const char *)buf);
  }
  return "";
}

//! returns if value could be set
bool com::Win32RegistryKey::set(const std::string& newValue) const
{
  HKEY hKey;
  DWORD dwDis;
  LONG result = RegCreateKeyEx(
                 topLevelKey(d_tl),
                 d_subKey.c_str(),
                 0,NULL,
                 REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL,
                 &hKey, &dwDis);
  if (result == ERROR_SUCCESS) {
     result = RegSetValueEx(
                 hKey,
                 0,
                 0,REG_SZ,
                 (const BYTE *)newValue.c_str(), newValue.size()+1);
     RegCloseKey(hKey);
  }
  return result == ERROR_SUCCESS;
}

//! remove key from registry
bool com::Win32RegistryKey::remove() const
{
  if (!exists())
    return true;
  HKEY key;
  if (open(key) == ERROR_SUCCESS) {
    LONG result = RegDeleteKey(key, "");
    RegCloseKey(key);
    return result == ERROR_SUCCESS;
  }

  return false;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
