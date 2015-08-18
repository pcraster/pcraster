#ifndef INCLUDED_COM_CLIBERROR
#define INCLUDED_COM_CLIBERROR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


namespace com {
  // ClibError declarations.
}

// defined in misc/error.c
// not included in misc.h since we ONLY can call it here
extern "C" void ResetError();

namespace com {

//! Translate old stle C error handling to throwing com::Exception's
/*!
  This class contains the implementation of to translate error handling
  done in the old PCRaster Ansi C libraries done in old code by calling
  Error() from the misc library. The code here enables one to translate
  that mechanism in throwing com::Exception's.

  For use see libs/pcrme/calc_globallibdefs.h

  All methods are static, there is no point in doing difficult while 
  all what we want is to "grep" from libmisc are global 
  thread-unsafe error buffers.

*/
class ClibError
{

  friend class ClibErrorTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ClibError&           operator=           (ClibError const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClibError               (ClibError const& rhs);

  static std::string      d_libErrorStr;
  static bool             d_errorHandlerCalled;

  static void HandleLibError(const char *msg);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /*! \todo hide this one make ClibErrorPrivate a friend and let
   *  construct the singleton
   */
                   ClibError               ();

  /* virtual */    ~ClibError              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  inline static void              reset           ();
  inline static void              libError        (const std::string& msg);
  inline static void              throwLibError   ();
  inline static std::string       getLibError     ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! clear state a fresh constucted ClibError object
void com::ClibError::reset()
{
  d_libErrorStr.clear();
  d_errorHandlerCalled=false;

  // init libmisc error stuff
  exitOnError =0;
  errorPrefixMsg = "";
  // extern int errorNestLevel;
  // errorNestLevel=0;

  // register to libmisc::errorHandler
  errorHandler = HandleLibError;

}

//! throw a com::Exception as result of error handling nested errors
/*!
 *  append already nested function calls.
 */
void com::ClibError::libError(const std::string& msg)
{
  // now let old style Error function call HandleLibError
  if(!d_errorHandlerCalled)
    Error(msg.c_str());
  else
    d_libErrorStr = msg + d_libErrorStr;

  throwLibError();
}


void com::ClibError::throwLibError(void)
{
  d_errorHandlerCalled = false;

  std::string str(d_libErrorStr);
  d_libErrorStr.clear();


  ResetError();
  // now throw what is stored
  throw com::Exception(str);
}

//! return lib error string
std::string com::ClibError::getLibError(void)
{
  d_errorHandlerCalled = false;
  return d_libErrorStr;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
