#ifndef INCLUDED_DAL_LIBRARY
#define INCLUDED_DAL_LIBRARY



// Library headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CACHE
#include "dal_Cache.h"
#define INCLUDED_DAL_CACHE
#endif

#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_ENVIRONMENT
#include "dal_Environment.h"
#define INCLUDED_DAL_ENVIRONMENT
#endif

#ifndef INCLUDED_DAL_FEATURELAYERGEOMETRIES
#include "dal_FeatureLayerGeometries.h"
#define INCLUDED_DAL_FEATURELAYERGEOMETRIES
#endif

#ifndef INCLUDED_DAL_FORMATS
#include "dal_Formats.h"
#define INCLUDED_DAL_FORMATS
#endif

#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#include "dal_MemoryDataPool.h"
#define INCLUDED_DAL_MEMORYDATAPOOL
#endif



namespace dal {
  // Library declarations.
  class Library;
}



namespace dal {

//! get global singleton object which can be queried for information.
Library* library();



//! Upon creation a Library object initialises the library.
/*!
  The lifetime of a Library object is managed by the Client class. See
  its documentation for more info.
*/
class PCR_DAL_DECL Library: private boost::noncopyable
{

  friend class LibraryTest;
  friend class Client;

private:

  Environment      _environment;

  bool             _cacheDatasetInfo;

  //! Formats the library has heard of.
  Formats          _knownFormats;

  //! Data which is stored in memory for communication purposes.
  MemoryDataPool   _memoryDataPool;

  //! Data which is stored in memory for speed optimization purposes.
  MemoryDataPool   _cacheDataPool;

  //! Cache for storing reusable and refcounted geometries objects.
  Cache<FeatureLayerGeometries> _geometriesCache;

                   Library             (boost::filesystem::path const& prefix,
                                        bool cacheDatasetInfo);

  void             addGraphicsFormats  ();

  void             addAttributeFormats ();

  static void      initialise          (boost::filesystem::path const& prefix,
                                        bool cacheDatasetInfo);

  static void      cleanUp             ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~Library            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static bool      isInitialised       ();

  Environment const& environment       () const;

  bool             cacheDatasetInfo    () const;

  Formats const&   knownFormats        () const;

  MemoryDataPool&  memoryDataPool      ();

  MemoryDataPool&  cacheDataPool       ();

  Cache<FeatureLayerGeometries>& geometriesCache();

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
