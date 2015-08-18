#ifndef INCLUDED_COM_CATCHALLEXCEPTIONS
#define INCLUDED_COM_CATCHALLEXCEPTIONS

/*
 * macros's to catch all kinds of errors
 * including win32 Os-errors
 * pcrcalc has example use
 */
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_EXCEPTION
#include <exception>
#define INCLUDED_EXCEPTION
#endif
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.

/*
 * zie ook boost::execution_monitor voor code ideen over SEH
 */

/* NON-Standard exception identification info
 * #ifdef BORLANDC
 *  foo() {
 *   // compiling with -xp on bcc32 will reveal this
 *     std::cerr << __throwExceptionName << "|"
 *               << __throwFileName      << "|"
 *               << __throwLineNumber    <<"\n";
 *   }
 *   within std::set_terminate(foo);
 * #endif
 *
 *  http://gcc.gnu.org/onlinedocs/libstdc++/19_diagnostics/howto.html
 *  std::set_terminate (__gnu_cxx::__verbose_terminate_handler);
 *
 */


#ifdef WIN32

# ifdef CPP_UNIT_COM_EXCEPTION_HANDLING
   // exception is for developer
#  define WIN32_EXCEP_STR "win32 exception not handled (see com_catchallexceptions.h)"
# else
#  ifndef INCLUDED_COM_WIN32
#   include "com_win32.h"
#  define INCLUDED_COM_WIN32
#  endif
   // pass exception description to user
#  define WIN32_EXCEP_STR com::win32ExceptionString(GetExceptionCode())
# endif

# define TRY_ALL                 try
/* was OS_EXCEPTION but can not combine catch and except in one function */
# define SEH(errStr)                                            \
   __except(EXCEPTION_EXECUTE_HANDLER) {                        \
    errStr << "ERROR System exception:\n "                      \
           << com::win32ExceptionString(GetExceptionCode())     \
           << std::endl;                                        \
  } }
#else

# define TRY_ALL                 try

#endif

# define OS_EXCEPTION(errStr)

// _MSC_VER TODO
//
// #ifdef WIN32
// # ifndef __MINGW32__ /* NO SEH support */
// #  undef  TRY_ALL
// #  define TRY_ALL                 try { try
// #  undef  OS_EXCEPTION
// #  define OS_EXCEPTION(stream)                                   X
//    __except(EXCEPTION_EXECUTE_HANDLER) {                         X
//      stream << "ERROR System exception:\n "                      X
//             << WIN32_EXCEP_STR                                   X
//             << std::endl;                                        X
//    } }
// # endif
// #endif
//


#ifdef CPP_UNIT_COM_EXCEPTION_HANDLING
#define COM_EXCEPTION_MESSAGES cEx.messagesInLine()
#else
#define COM_EXCEPTION_MESSAGES cEx.messages()
#endif

/*!
 * catch all possible exceptions and put them in stream
 */

#ifdef GCC
// gcc/cxxabi.h
// Returns the type_info for the currently handled exception [15.3/8], or
extern "C" std::type_info *__cxa_current_exception_type ();
#define PRINT_EXCEPTION_TYPE(stream) \
    std::type_info *t=__cxa_current_exception_type ();          \
    if (t)                                                      \
      stream << " typeinfo::name = " << t->name() << std::endl;
#else
#define PRINT_EXCEPTION_TYPE(stream) \
      stream << " typeinfo::name = " << "not present " << std::endl;
#endif

#define CATCH_ALL(stream,prefix,appSpecificCatch)  \
 OS_EXCEPTION(stream)                           \
 appSpecificCatch                               \
 catch (const com::Exception& cEx) {            \
  stream<<prefix<< COM_EXCEPTION_MESSAGES;      \
 } catch (const dal::Exception& dEx) {          \
  stream<<prefix<< dEx.message();               \
 } catch (const std::bad_alloc &) {             \
  stream<<prefix<<"Not enough memory\n";        \
 } catch (const std::exception &msg) {          \
  stream<<prefix<<"Unknown exception: "         \
    << msg.what() << " (programming error)\n";  \
 } catch (...) {                                \
  stream<<prefix<<"Unknown exception (programming error)\n";  \
    PRINT_EXCEPTION_TYPE(stream)                              \
 }


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



#endif
