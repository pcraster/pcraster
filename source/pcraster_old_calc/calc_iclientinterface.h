#ifndef INCLUDED_OLDCALC_ICLIENTINTERFACE
#define INCLUDED_OLDCALC_ICLIENTINTERFACE

#include "stddefx.h"
#include "calc_executescriptstatus.h"

#include <sstream>


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

  ClientInterface *d_ci{nullptr};
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
