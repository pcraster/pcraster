#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifdef WIN32

# ifndef INCLUDED_COM_WIN32
#  include "com_win32.h"
#  define INCLUDED_COM_WIN32
# endif

#else

# ifndef INCLUDED_DLFCN
#  include <dlfcn.h>
#  define INCLUDED_DLFCN
# endif
bool com::DynamicLibrary::checkError() const
{
  /* see dlopen(3) all dl-funcs set dlerror
   * dlerror will reset after call
   * so cache it in d_dllError
   */
  d_dllError = ::dlerror();
  return d_dllError;
}

#endif

//! find and load  dynamic library named \a libNameNoExt
/*! 
   library name is given without the platform extension (.dll,.so).
   On win32 GetModuleName is tried first to see if dll is already loaded;
   wasAlreadyLoaded is set accordingly.
  \throws com::DynamicLibraryException if lib not found or other error
*/
com::DynamicLibrary::DynamicLibrary(
 const std::string& libNameNoExt):
  d_libName(libNameNoExt),
  d_wasAlreadyLoaded(false)
{
#ifdef WIN32
  bool failure(false);
  d_dllHandle = ::GetModuleHandle(nativeLibName().c_str());
  d_wasAlreadyLoaded = d_dllHandle != NULL;
  if (!d_wasAlreadyLoaded) {
   d_dllHandle = ::LoadLibrary(nativeLibName().c_str());
   failure = ((unsigned)(d_dllHandle) <= HINSTANCE_ERROR);

  }
#else
  d_dllHandle=  ::dlopen(nativeLibName().c_str(),RTLD_NOW);
  bool failure = checkError();
#endif

  if (failure)
    throwException();
}

//! unload the library
com::DynamicLibrary::~DynamicLibrary()
{
#ifdef WIN32
  if (!d_wasAlreadyLoaded) // no need to unload
   if (!::FreeLibrary(d_dllHandle))
     throwException();
#else
  ::dlclose(d_dllHandle);
  if (checkError())
    throwException();
#endif
}

std::string com::DynamicLibrary::osError() const
{
#ifdef WIN32
  return win32GetLastError();
#else
  return d_dllError;
#endif
}

std::string com::DynamicLibrary::nativeLibName()const
{
  com::PathName libPath(d_libName);
  std::string base = libPath.baseName();
#ifdef WIN32
  com::PathName baseName(base+".dll");
#elif __APPLE__
  com::PathName baseName("lib"+base+".dylib");
#else
  com::PathName baseName("lib"+base+".so");
#endif
  com::PathName path(libPath.directoryName());
  path += baseName;
  path.makeNative();
  return path.toString();
}

void com::DynamicLibrary::throwException(const std::string& symbol)const
{
 std::ostringstream str;
 str << "Library "
     << nativeLibName()
     << ": ";
 if (!symbol.empty())
    str << "symbol "
        <<  symbol
        << ": ";
 str << osError();
 throw com::DynamicLibraryException(str.str());
}



//! Returns the address of the symbol with name \a symbolName.
/*!
  \param     symbolName Name of symbol to look for.
  \return    Address or 0.
  \todo      Still unsure on win32 linking, FTTB  check both (_)?name
*/
void* com::DynamicLibrary::address(const std::string& symbolName) const
{
#ifdef WIN32
  void *addr = (void *)GetProcAddress(d_dllHandle,symbolName.c_str());
  if(!addr) { // FTTB
    std::string s2("_");
    s2 += symbolName;
    addr = (void *)GetProcAddress(d_dllHandle,s2.c_str());
  }
#else
  void *addr = ::dlsym(d_dllHandle,symbolName.c_str());
  if(checkError()) {
    addr = 0;
  }
#endif

  return addr;
}
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

/*! quick hack to find directory where library is found, by searching
 *  symbol and setting the directory if found
 */
void* com::DynamicLibrary::addressAndSetLibPath(const std::string& symbolName)
{
  void *addr = address(symbolName);
  if (!addr)
    return 0;

  d_directory.clear();
#ifdef WIN32
  char dllFileNameCstr[2048];
  GetModuleFileName(d_dllHandle,dllFileNameCstr,2047);
  d_directory = com::PathName(dllFileNameCstr).directoryName();
#else
// #ifndef _GNU_SOURCE
// #error   linux requires GNU compiler for dladdr extension
// #endif
  Dl_info loc;
  if (dladdr(addr,&loc)) {
    d_directory=loc.dli_fname;
  }  else {
    assert(false);
  }
  d_directory = com::PathName(d_directory).directoryName();
#endif

  return addr;
}



//! Returns true if a symbol with name \a symbolName exists in the library.
/*!
  \param     symbolName Name of symbol to look for.
  \return    true or false.
*/
bool com::DynamicLibrary::hasSymbol(const std::string& symbolName) const
{
  return address(symbolName) != 0;
}



//! load a symbol named \a symName
/*!
  \throws com::DynamicLibraryException if \a symName not found or other error
*/
void *com::DynamicLibrary::loadSymbol(
  const std::string& symName) const
{
  void* addr = address(symName);

  if(!addr) {
    throwException(symName);
  }
  return addr;
}

//! load a function named \a symName
/*! simply a const wrapper around loadSymbol()
 */
const void *com::DynamicLibrary::loadFunction(
  const std::string& symName) const
{
  return loadSymbol(symName);
}
