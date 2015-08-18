#ifndef INCLUDED_COM_APPARGS
#define INCLUDED_COM_APPARGS

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace com {


//! The AppArgs builds a main argv/argc style argument array from a string
/*!
 * It does NOT '- and "-quoting to keep whitespace in arguments.
 * argv will be terminated by an additional 0 ptr in conformance for
 * passing to execvp(3) and the like. Hence argv()[argc()] == 0
 */
class AppArgs {
private:
  size_t  d_argc;
  char  **d_argv;
  char   *d_buffer;
  void             init                (const std::string& allArgs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AppArgs             (const std::string& args0, 
                                        const std::string& otherArgs);

                   AppArgs             (const std::string& allArgs);

  /* virtual */   ~AppArgs             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  int              argc                () const;
  char **          argv                () const; 
};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
