#ifndef INCLUDED_CALC_ICLIENTINTERFACE
#define INCLUDED_CALC_ICLIENTINTERFACE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_EXECUTESCRIPTSTATUS
#include "calc_executescriptstatus.h"
#define INCLUDED_CALC_EXECUTESCRIPTSTATUS
#endif

namespace calc {
  // IClientInterface declarations.
    class ClientInterface;
    class ProgressCallBack;
}

namespace com {
  class PathName;
}

namespace calc {



//! interface for 3th parties
class IClientInterface
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IClientInterface&           operator=           (const IClientInterface&);

  //! Copy constructor. NOT IMPLEMENTED.
                   IClientInterface               (const IClientInterface&);

  ClientInterface *d_ci;
  //! empty if no error;
  mutable std::ostringstream  d_errorStream;

  //! buffer for error stream
  /*!
   * must have a string buffer so we can return a reference
   * that can call c_str() to this existing buffer
   */
  mutable std::string         d_errorMsg;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IClientInterface               ();

  /* virtual */    ~IClientInterface              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void run();

  void setProgressCallBack(ProgressCallBack *pcb);
  void setRunDirectory(const com::PathName&  rd);
  void setScriptFile(const com::PathName& scriptName);

  void setError(const char *msg);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ExecuteScriptStatus executeScriptStatus() const;
  bool                error()               const;
  const std::string&  errorMessage()        const;

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



} // namespace calc

#endif
