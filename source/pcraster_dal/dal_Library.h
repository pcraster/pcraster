#ifndef INCLUDED_DAL_LIBRARY
#define INCLUDED_DAL_LIBRARY

#include "dev_Compiler.h"
#include "dal_Cache.h"
#include "dal_Configure.h"
#include "dal_Environment.h"
#include "dal_FeatureLayerGeometries.h"
#include "dal_Formats.h"
#include "dal_MemoryDataPool.h"

#include <filesystem>


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
class PCR_DAL_DECL Library
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

                   Library             (std::filesystem::path const& prefix,
                                        bool cacheDatasetInfo);

  void             addGraphicsFormats  ();

  void             addAttributeFormats ();

  static void      initialise          (std::filesystem::path const& prefix,
                                        bool cacheDatasetInfo);

  static void      cleanUp             ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Library             (Library const& other) = delete;

  Library&         operator=           (Library const& other) = delete;

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
