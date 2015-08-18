#ifndef INCLUDED_DEV_PYTHONCLIENT
#define INCLUDED_DEV_PYTHONCLIENT

// External headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.
#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

// Module headers.



namespace dev {
  // PythonClient declarations.
}



namespace dev {

//! Utility class for clients that use the Python library.
/*!
  Creating a PythonClient object will initialize the Python library. Once
  the object goes out of scope the matching terminate will be called.

  It is possible to instantiate more than one PythonClient objects in the
  same application.

  Unfortunately, not all memory will be freed.
  - http://bugs.python.org/issue1445210
*/
class PythonClient: private boost::noncopyable
{

  friend class PythonClientTest;

private:

  //! Number of times Python is initialized without being terminated.
  static unsigned short _count;

  //! Whether initialization of the Python library succeeded.
  bool             _initialized;

protected:

                   PythonClient        ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~PythonClient       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isInitialized       () const;

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

} // namespace dev

#endif
