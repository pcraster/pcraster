#ifndef INCLUDED_DEV_GDALCLIENT
#define INCLUDED_DEV_GDALCLIENT

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
  // GDalClient declarations.
}



namespace dev {

//! Manages the initiazation and cleaning up of the GDal library, if necessary.
/*!
  The GDal library needs initialization before it can be used. Afterwards, it
  needs some cleaning up to free up resources.

  This class can be used multiple times in an application. Only when the
  GDal library is not initialized yet, will the constructor initialize it.
  Once the last GDalClient object goes out of scope, the GDal library will be
  'cleaned'.

  It is possible that someone else has already initialized the GDal library.
  In that case, we are not in control, and we should not initialize and
  clean the GDal library.

  It is assumed that if no driver is registered yet by the GDALDriverManager,
  that we are in control.

  \sa        .
*/
class GDalClient: private boost::noncopyable
{

  friend class GDalClientTest;

private:

  //! Whether we initialized the GDal library or not.
  static bool      _weInitializedGdal;

  //! Number of times library is initialized without being terminated.
  static unsigned short _count;

protected:

                   GDalClient          ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~GDalClient         ();

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
