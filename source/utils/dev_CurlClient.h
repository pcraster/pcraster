#ifndef INCLUDED_DEV_CURLCLIENT
#define INCLUDED_DEV_CURLCLIENT

// External headers.
#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.



namespace dev {
  // CurlClient declarations.
}



namespace dev {

//! Utility class for clients that use the Curl library.
/*!
  Creating a CurlClient object will initialize the Curl library. Once
  the object goes out of scope the matching clean up will be called.

  The Curl library can be initialized multiple times.
*/
class CurlClient: private boost::noncopyable
{

  friend class CurlClientTest;

private:

  //! Number of times the library is initialized without being cleaned.
  static unsigned short d_count;

  //! Whether initialization of the library succeeded.
  bool             d_initialized;

protected:

                   CurlClient          ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~CurlClient         ();

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
