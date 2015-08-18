// This one should be the first thing that gets included. Otherwise we get
// a couple of warnings, eg:
// warning: "_POSIX_C_SOURCE" redefined
#ifndef INCLUDED_DEV_INCLUDEPYTHONAPI
#include "dev_IncludePythonApi.h"
#define INCLUDED_DEV_INCLUDEPYTHONAPI
#endif

#ifndef INCLUDED_DEV_PYTHONCLIENT
#include "dev_PythonClient.h"
#define INCLUDED_DEV_PYTHONCLIENT
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the PythonClient class.
*/



namespace dev {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PYTHONCLIENT MEMBERS
//------------------------------------------------------------------------------

unsigned short PythonClient::_count = 0;



//------------------------------------------------------------------------------
// DEFINITION OF PYTHONCLIENT MEMBERS
//------------------------------------------------------------------------------

PythonClient::PythonClient()

  : _initialized(false)

{
  // Python docs: There is no return value; it is a fatal error if the
  // initialization fails.
  Py_Initialize();
  _initialized = true;
  ++_count;
}



PythonClient::~PythonClient()
{
  if(_initialized) {
    assert(_count > 0);
    // Python docs: There is no return value; errors during finalization are
    // ignored.
    Py_Finalize();
    --_count;
  }
}



bool PythonClient::isInitialized() const
{
  return _initialized;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

