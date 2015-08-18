#ifndef INCLUDED_COM_DYNAMICLIBRARY
#define INCLUDED_COM_DYNAMICLIBRARY

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifdef WIN32
# ifndef INCLUDED_WINDOWS
#  include <windows.h>
# define INCLUDED_WINDOWS
# endif
#endif


namespace com {
//! exception throw by com::DynamicLibrary object
class DynamicLibraryException: public Exception {
 public:
  DynamicLibraryException(const std::string& msg):
   Exception(msg) {};
 };


// some macro magic to get entry points
#define STATIC_DLL_FUNC_PTR(dll,funcName) \
  static T_##funcName funcPtr = 0; \
  if (!funcPtr) \
   funcPtr = (T_##funcName)(dll->loadFunction(#funcName));

//! Load a shared (unix) or dynamic (win32) library on demand
/*! any method, constructor and destructor can throw
 *   com::DynamicLibraryException in case of error
 *  A (later) alternative is Qt's QLibrary. DynamicLibrary seems to
 *  have clearer exception handling.
 *
 *  \todo      Still unsure on win32 linking, FTTB  check both 
 *             (_)name. Thus do not rely on distinction on leading
 *             underscore.
 *
 */
class DynamicLibrary : boost::noncopyable
{
private:
#ifdef WIN32
  typedef HINSTANCE  DllHandle;
#else
  typedef void *     DllHandle;
  mutable const   char *d_dllError;
  bool               checkError() const;
#endif

  mutable DllHandle   d_dllHandle;
  const std::string   d_libName;
  // quite a hack, set for win32 in ctor, for linux in addressAndSetLibPath
  std::string         d_directory;
  bool                d_wasAlreadyLoaded;

  std::string osError()       const;
  std::string nativeLibName() const;
  void        throwException  (const std::string& symbol="") const;

public:
  // CREATORS
  DynamicLibrary(const std::string& libNameNoExt);

  ~DynamicLibrary(); 

  void*            addressAndSetLibPath(const std::string& symbolName);

  // ACCESSORS
  bool             hasSymbol           (const std::string& symbolName) const;

  void*            address             (const std::string& symbolName) const;

  void *loadSymbol(const std::string& symName) const;

  const void *loadFunction(const std::string& symName) const;

  //! return the handle of the library
  const DllHandle libHandle() const { return d_dllHandle; };

  //! return if dll was already found in address space (STILL TO TEST!)
  bool wasAlreadyLoaded() const { return d_wasAlreadyLoaded; }

  std::string directory() const { return d_directory;}
};

} // namespace com

#endif
