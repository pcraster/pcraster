#ifndef INCLUDED_COM_WIN32REGISTRYKEY
#define INCLUDED_COM_WIN32REGISTRYKEY

#include "stddefx.h"

#include <windows.h>
#include <string>



namespace com {
  // Win32RegistryKey declarations.
}



namespace com {



//! A win32 registry key
/*!
   only string types as default key values are supported
   \todo
    A lot of todo concerning read/write access and so on,
     especially on NT
*/
class Win32RegistryKey
{
public:
  enum TopLevel {
    CurrentUser,
    LocalMachine
  };

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Win32RegistryKey&           operator=           (const Win32RegistryKey&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Win32RegistryKey               (const Win32RegistryKey&);

   TopLevel     d_tl;
   std::string  d_subKey;

   LONG         open(HKEY& key) const;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Win32RegistryKey              (TopLevel tl,
                                                  const std::string&  subKey);

  /* virtual */   ~Win32RegistryKey              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool        exists() const;
  std::string value()  const;
  std::string value(const std::string& name) const;
  bool        set(const std::string& newValue) const;
  bool        remove() const;
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
