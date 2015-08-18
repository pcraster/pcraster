#ifndef INCLUDED_DAL_CLIENT
#define INCLUDED_DAL_CLIENT



// External headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // Client declarations.
  class Dal;
  class Library;
}



namespace dal {

//! Objects of this class do required initialisation of the Dal library.
/*!
  The Dal library can not be used before some initialization has been done.
  By instantiating this Client class this initialization is done. At
  destruction the library is de-initialized.

  It is safe to use this class more than once (in series or in parallel).
  The library will be initialized once, unless it has been de-initialized.

  \warning   Although the Dal library depends on Qt and GDal, this class does
             not initialize these libraries. You need to do this yourself.
             Have a look at dev::QtClient and dev::GDalClient. More support
             libraries may need initialisation.
*/
class PCR_DAL_DECL Client: private boost::noncopyable
{

  friend class ClientTest;

private:

  //! Number of times dal is initialized without being terminated.
  static size_t    _count;

  static Dal*      _dal;

protected:

                   Client              (boost::filesystem::path const& prefix,
                                        bool addAllDrivers=false,
                                        bool cacheDatasetInfo=true);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Client             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static Dal&      dal                 ();

  static bool      isInitialized       ();

  static Library&  library             ();

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

} // namespace dal

#endif
