#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_APP_OPTIONS
#include "app_options.h"
#define INCLUDED_APP_OPTIONS
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

// Module headers.
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif


/*!
  \file
  C++ in support of appinst.c
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPTIONS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF OPTIONS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


/*!
 * in need of pcrcalc/test224a, repetive use of strtok was no good!
 */
extern "C" BOOL app_setDynamicLibraries(
  const char *flag)
{
  std::vector<std::string> libList(com::split(flag,':'));
  PRECOND(libList.size() >= 1);
  PRECOND(libList[0] == "dynamiclibraries");
  for (size_t i=1;i < libList.size(); i++) {
   if (nrDynamicLibraryNames == 64)
    return RetError(FALSE,"Too many dynamic libraries (max=%d)",64);
   dynamicLibraryNames[nrDynamicLibraryNames++] =
     StrcpyChkMalloc(libList[i].c_str());
  }
  return TRUE;
}

typedef struct SAVE_STRTOK_IMPL {
  size_t d_next;
  std::vector<std::string> d_strings;
} SAVE_STRTOK_IMPL;

SAVE_STRTOK createSaveStrtok(const char *str)
{
  SAVE_STRTOK s;
  s.data = new SAVE_STRTOK_IMPL();
  s.data->d_strings=com::split(str);
  s.data->d_next=0;
  return s;
}

void deleteSaveStrtok(SAVE_STRTOK s)
{
  delete s.data;
}

const char* nextSaveStrtok(SAVE_STRTOK s)
{
  if (s.data->d_next <  s.data->d_strings.size())
   return s.data->d_strings[s.data->d_next++].c_str();
  return 0;
}
